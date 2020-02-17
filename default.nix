let
  pkgs = import <nixpkgs> {};
  source = pkgs.lib.sourceByRegex ./. [
    "^vimish-fold\.el$"
    "^test.*$"
  ];
in
pkgs.stdenv.mkDerivation {
  name = "vimish-fold";
  src = source;
  buildInputs = [
    (pkgs.emacs26WithPackages (epkgs: [epkgs.f epkgs.ert-runner]))
  ];
  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';
  installPhase = ''
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
  '';
  checkPhase = ''
    emacs -L . --batch --eval "(progn (require 'ert-runner) (ert-run-tests t (lambda (x) nil)))"
  '';
  doCheck = true;
}
