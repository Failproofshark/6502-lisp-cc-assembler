(in-package :cl-user)
(defpackage :6502-lisp-cc-assembler
  (:use :cl
        :cl-ppcre))
(in-package :6502-lisp-cc-assembler)

(declaim (optimize (debug 3)))

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
;; TODO need to downcase all characters being read in and tokenized
;; TODO tokenize should accept a stream but it should not be responsible for setting up "with-open-file". Rather some external function should call tokenize within a with-open-file macro
(defun tokenize (stream)
  (with-open-file (source-code stream)
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
               
              (t (write-char character pending-token))))
      (finish-output pending-token)
      (nreverse tokens))))

;; todo In a two pass assembly process where we look for things like labels we may need an intermediary instead of directly writing the object code to the file
;; TODO need to check for syntax errors (e.g. unsupported address modes, malformed data etc)
;; TODO general process is similar to mnemonic opcode table generation: read opcode read argumements (anyhwere between 1 and 3 items, e.g. it's just an address but could also be address "," [x,y]. The most important bit to further process is the address. If it's 4 characters we need to split it up over two bytes in low byte high byte order
;; TODO (note) adding expressions will change the way we translate addresses as we can have mathematical operators between parentheses
;; TODO accept optional output stream argument
(defun assemble (tokens)
  ;; TODO rename convert-address to something more appropriate
  ;; TODO add "is-indirect-p" argument to convert to initiate whether or not we are indirect addressing or not (all indirect address modes enclose their arguments in parens
  (labels ((convert-address (address-string &key indirect-mode register)
             (cond ((char= (aref address-string 0) #\#)
                    `("-immediate" ,(parse-integer address-string :start 2 :radix 16)))
                   ((char= (aref address-string 0) #\a)
                    `("-accumulator"))
                   ((char= (aref address-string 0) #\$)
                    (if (= (length address-string) 3)
                        `("-zero-page" ,(parse-integer address-string :start 1 :radix 16))
                        `(,(cond (indirect-mode "-indirect")
                                 ((equal "x" register) "-absolute-x")
                                 ((equal "y" register) "-absolute-y")
                                 (t "-absolute"))
                          ,(parse-integer address-string :start 3 :radix 16) ,(parse-integer address-string :start 1 :end 3 :radix 16))))))
           (convert (tokens)
             ;; TODO handle branch opcodes separately
             (cond ((= 1 (length tokens))
                    `(,(gethash (car tokens) mnemonic-opcode-lookup)))
                   ((= 2 (length tokens))
                    (let* ((converted-address-and-mode (convert-address (cadr tokens)))
                           (opcode (car tokens))
                           (completed-code (concatenate 'string opcode (car converted-address-and-mode))))
                      (append 'nil `(,(gethash completed-code mnemonic-opcode-lookup)) (cdr converted-address-and-mode))))
                   ((= 4 (length tokens))
                    (let* ((converted-address-and-mode (if (equal "(" (cadr tokens))
                                                           (convert-address (caddr tokens) :indirect-mode t)
                                                           (convert-address (cadr tokens) :register (cadddr tokens))))
                           (opcode (car tokens))
                           (completed-code (concatenate 'string opcode (car converted-address-and-mode))))
                      (break)
                      (append 'nil `(,(gethash completed-code mnemonic-opcode-lookup)) (cdr converted-address-and-mode)))))))
    (let ((buffer 'nil)
          (object-code 'nil))
      (loop for token = (car tokens) while token do
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
      object-code)))
