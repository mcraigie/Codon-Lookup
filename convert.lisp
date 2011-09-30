(defun output-codons-to-csv (input file-path append-stop)
  (let (csv-line)
    (if append-stop
	(setf input (concatenate 'string (replace-all input "!" "") "!")))
    (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop for n from 0 to 6 do
	   (dolist (sub (get-codon-results input))
	     (push (nth n sub) csv-line))
	   (setf csv-line (substitute "" nil csv-line :TEST #'EQUAL));put this here to deal with a null handling error in the princ-csv function
	   (princ-csv csv-line stream)
	   (setf csv-line nil)))))

(defun get-codon-results (input)
  (let (results lookup-result)
    (loop for i from '0 to (- (length input) 1) do
	 (setf lookup-result (dna-codon-lookup (char input i)))
	 (if lookup-result (push lookup-result results)))
    results))

(defun dna-codon-lookup (x)
  (case x
    (#\A '(A GCT GCC GCA GCG));Alanine
    (#\C '(C TGT TGC));Cysteine
    (#\D '(D GAT GAC));Aspartic acid
    (#\E '(E GAA GAG));Glutamic acid
    (#\F '(F TTT TTC));Phenylalanine
    (#\G '(G GGT GGC GGA GGG));Glycine
    (#\H '(H CAT CAC));Histidine
    (#\I '(I ATT ATC ATA));Isoleucine
    (#\K '(K AAA AAG));Lysine
    (#\L '(L TTA TTG CTT CTC CTA CTG));Leucine
    (#\M '(M ATG));Methionine, Start
    (#\N '(N AAT AAC));Asparagine
    (#\P '(P CCT CCC CCA CCG));Proline
    (#\Q '(Q CAA CAG));Glutamine
    (#\R '(R CGT CGC CGA CGG AGA AGG));Arginine
    (#\S '(S TCT TCC TCA TCG AGT AGC));Serine
    (#\T '(T ACT ACC ACA ACG));Threonine
    (#\V '(V GTT GTC GTA GTG));Valine
    (#\W '(W TGG));Tryptophan
    (#\Y '(Y TAT TAC));Tyrosine
    (#\! '(STOP TAA TGA TAG))));Ochre, Opal, Amber, Stop

;------- Helper Functions -------

(defun princ-csv (items csv-stream
                  &key (quote #\")
                       (separator #\,)
                       (ignore-nulls nil)
                       (newline (string #\Newline))
                       (princ #'princ-to-string))
  "Write the list ITEMS to csv-stream."
  (flet ((write-word (word)
           (write-char quote csv-stream)
           (loop
              for char across (funcall princ word)
              if (char= quote char) do
                (progn
                  (write-char quote csv-stream)
                  (write-char quote csv-stream))
              else do
                (write-char char csv-stream))
           (write-char quote csv-stream)))
    (when items
      (write-word (car items))
      (dolist (i (cdr items))
        (write-char separator csv-stream)
        (if ignore-nulls
            (when (not (null i))
              (write-word i))
            (write-word i)))
      (write-sequence newline csv-stream))))
	;; Copyright (c) 2002-2006, Edward Marco Baringer
	;; All rights reserved. 
	;; 
	;; Redistribution and use in source and binary forms, with or without
	;; modification, are permitted provided that the following conditions are
	;; met:
	;; 
	;;  - Redistributions of source code must retain the above copyright
	;;    notice, this list of conditions and the following disclaimer.
	;; 
	;;  - Redistributions in binary form must reproduce the above copyright
	;;    notice, this list of conditions and the following disclaimer in the
	;;    documentation and/or other materials provided with the distribution.
	;;
	;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
	;;    of its contributors may be used to endorse or promote products
	;;    derived from this software without specific prior written permission.
	;; 
	;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
	;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
	;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
	;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
	;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
	;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
	;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
	;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
	;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
;from http://cl-cookbook.sourceforge.net/strings.html
;Copyright © 2002-2007 The Common Lisp Cookbook Project
