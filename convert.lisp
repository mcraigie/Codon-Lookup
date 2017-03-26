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
