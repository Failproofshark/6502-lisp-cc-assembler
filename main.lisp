(in-package :cl-user)
(defpackage :6502-lisp-cc-assembler
  (:use :cl
        :cl-ppcre))
(in-package :6502-lisp-cc-assembler)

;; Syntax is based on turbo asm
;; TODO giant table or keep the opcode look up on separate tables?
(defvar mnemonic-opcode-lookup
  (let ((mnemonic-opcode-pairs (with-open-file (stream #P"opcodes.txt")
                                 (loop for pair = (read-line stream 'nil 'nil) while pair collect
                                                                                          (cl-ppcre:split "\\s" pair))))
        (new-table (make-hash-table :test 'equalp)))
    (loop for mnemonic-opcode-pair = (car mnemonic-opcode-pairs) while mnemonic-opcode-pairs do
      (setf (gethash (car mnemonic-opcode-pair) new-table) (parse-integer (cadr mnemonic-opcode-pair) :radix 16))
      (setf mnemonic-opcode-pairs (cdr mnemonic-opcode-pairs)))
    new-table))

(defvar available-mnemonics (let ((new-table (make-hash-table :test 'equalp)))
                              (with-open-file (listing #P "mnemonic_listing.txt")
                                (loop for mnemonic = (read-line listing 'nil 'nil) while mnemonic do
                                  (setf (gethash mnemonic new-table) 1)))
                              new-table))

;; TODO add support for more useful features
;; Use in-place modification version of cons
(defun tokenize (source-code)
  (let ((tokens 'nil)
        (new-token 'nil)
        (pending-token (make-string-output-stream)))
    (loop for character = (read-char source-code 'nil 'nil) while character do
      (cond ((or (char= #\, character)
                 (char= #\+ character)
                 (char= #\- character)
                 (char= #\* character)
                 (char= #\/ character)
                 (char= #\( character)
                 (char= #\) character))
             ;; technically it's own token so if we were in the middle of reading one token and no space or break occurs we need to record what was in our buffer and this new character
             ;; TODO move this to it's own function
             (progn 
               (setf new-token (get-output-stream-string pending-token))
               (if (> (length new-token) 0)
                   (setf tokens (cons new-token tokens)))
               (setf tokens (cons (format 'nil "~a" character) tokens))))
            
            ((or (char= character #\Newline)
                 (char= character #\Space)
                 (char= character #\Tab))
             (progn 
               (setf new-token (get-output-stream-string pending-token))
               (if (> (length new-token) 0)
                   (setf tokens (cons new-token tokens)))))
            
            (t (write-char (char-downcase character) pending-token))))
    (finish-output pending-token)
    (nreverse tokens)))

;; todo In a two pass assembly process where we look for things like labels we may need an intermediary instead of directly writing the object code to the file
;; TODO need to check for syntax errors (e.g. unsupported address modes, malformed data etc)
;; TODO general process is similar to mnemonic opcode table generation: read opcode read argumements (anyhwere between 1 and 3 items, e.g. it's just an address but could also be address "," [x,y]. The most important bit to further process is the address. If it's 4 characters we need to split it up over two bytes in low byte high byte order
;; TODO (note) adding expressions will change the way we translate addresses as we can have mathematical operators between parentheses
;; TODO accept optional output stream argument
;; TODO Having syntax checking occur in the first pass of our assembly process will allow us to eliminate the need of a buffer to hold our object code (we can instead simply write our assemblage directly to the file
(defun assemble (tokens)
  ;; TODO rename convert-address to something more appropriate
  (labels ((convert-address (address-string &key indirect-mode register only-convert-address)
             (cond ((char= (aref address-string 0) #\#)
                    `("-immediate" ,(parse-integer address-string :start 2 :radix 16)))
                   ((char= (aref address-string 0) #\a)
                    `("-accumulator"))
                   ((char= (aref address-string 0) #\$)
                    (append 'nil
                            `(,(if (not only-convert-address)
                                   (cond (indirect-mode "-indirect")
                                         ((equal "x" register) "-absolute-x")
                                         ((equal "y" register) "-absolute-y")
                                         ((= (length address-string) 3) "-zero-page")
                                         (t "-absolute"))))
                            (if (= (length address-string) 3)
                                `(,(parse-integer address-string :start 1 :radix 16))
                                 ;; Remember high byte low byte order
                                `(,(parse-integer address-string :start 3 :radix 16) ,(parse-integer address-string :start 1 :end 3 :radix 16)))))))
           (convert (tokens)
             ;; TODO handle branch opcodes separately
             ;; TODO refactor the 4 and 5 opcode length code blocks
             (cond ((= 1 (length tokens))
                    `(,(gethash (car tokens) mnemonic-opcode-lookup)))
                   ((= 2 (length tokens))
                    ;; TODO Need a better way to figure out how to deal with branch opcodes. A branching opcode
                    (let* ((is-branch-opcode (char= (aref (car tokens) 0) #\b))
                           (converted-address-and-mode (convert-address (cadr tokens) :only-convert-address is-branch-opcode))
                           (opcode (car tokens))
                           (completed-code (concatenate 'string opcode (car converted-address-and-mode))))
                      (append 'nil `(,(gethash completed-code mnemonic-opcode-lookup)) (cdr converted-address-and-mode))))
                   ((= 4 (length tokens))
                    (let* ((converted-address-and-mode (if (equal "(" (cadr tokens))
                                                           (convert-address (caddr tokens) :indirect-mode t)
                                                           (convert-address (cadr tokens) :register (cadddr tokens))))
                           (opcode (car tokens))
                           (completed-code (concatenate 'string opcode (car converted-address-and-mode))))
                      (append 'nil `(,(gethash completed-code mnemonic-opcode-lookup)) (cdr converted-address-and-mode))))
                   ((= 6 (length tokens))
                    (let* ((opcode (car tokens))
                           (completed-code (concatenate 'string opcode (if (equal (car (last tokens)) ")")                                                                                    "-indexed-indirect"
                                                                           "-indirect-indexed"))))
                      (append 'nil `(,(gethash completed-code mnemonic-opcode-lookup)) `(,(parse-integer (caddr tokens) :start 1 :radix 16))))))))
    (let ((buffer 'nil)
          (object-code 'nil))
      (loop for token = (car tokens) while token do
        ;;TODO lets use the destructive version of append
        (setf buffer (append buffer `(,token)))
        (if (or (gethash (cadr tokens) available-mnemonics)
                (= (length (cdr tokens)) 0))
            (progn
              (loop for byte in (convert buffer) do
                    (push byte object-code))
              (setf buffer 'nil)))
        ;; We could use the standard loop-over-list syntax however we would need to add an extra call to convert anything remaining in the buffer after the loop ended. So instead we manually move along the list
        (setf tokens (cdr tokens)))
      ;; we recorded the the buffer in reverse (pushing bytes) for speed purposes. We used append for the buffer read in because it doesn't grow as large as the final object code we wish to output
      (revappend object-code 'nil))))

(defun assemble-source-to-object-code (source-code-file output-file-name)
  (with-open-file (source-code source-code-file)
    (with-open-file (output-file output-file-name :element-type 'unsigned-byte :direction :output :if-does-not-exist :create)
      (loop for byte in (assemble (tokenize source-code)) do
        (write-byte byte output-file)))))
