;; Syntax is based on turbo asm
;; TODO giant table or keep the opcode look up on separate tables?
(defvar mnemonic-opcode-lookup (let ((tokenized-listing (tokenize #P"opcodes.txt"))
                                     (new-table (make-hash-table :test 'equalp)))
                                 (loop for mnemonic = (car foo)
                                       for opcode = (cadr foo) while foo do
                                         (setf (gethash mnemonic new-table) (parse-integer opcode :radix 16))
                                         (setf foo (cddr foo)))
                                 new-table))

;; We're going for a double pass assembler since that seems simpler to do; generating a table of address look-ups . May move to single pass
;; TODO add support for more useful features 
(defun tokenize (stream)
  (with-open-file (source-code stream)
    (let ((tokens 'nil)
          (new-token 'nil)
          (pending-token (make-string-output-stream)))
      (loop for character = (read-char source-code 'nil 'nil) while character do 
        (if (and (char/= character #\Space)
                 (char/= character #\Newline)
                 (char/= character #\Tab))
            (write-char character pending-token)
            (progn 
              (setf new-token (get-output-stream-string pending-token))
              (if (> (length new-token) 0)
                  (setf tokens (cons new-token tokens))))))
      (finish-output pending-token)
      (nreverse tokens))))

;; todo In a two pass assembly process where we look for things like labels we may need an intermediary instead of directly writing the object code to the file
;; TODO general process is similar to mnemonic opcode table generation: read opcode read argumements (anyhwere between 1 and 3 items, e.g. it's just an address but could also be address "," [x,y]. The most important bit to further process is the address. If it's 4 characters we need to split it up over two bytes in low byte high byte order
(defun assemble (stream)
  (let))



