(lang dune 3.7)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(authors "Vadim Yakshigulov <vadim.iakshigulov@gmail.com>")

(maintainers "Vadim Yakshigulov <vadim.iakshigulov@gmail.com>")

(bug_reports "https://github.com/toadharvard/asm-interpreter/issues")

(homepage "https://github.com/toadharvard/asm-interpreter")

(package
 (name asm)
 (synopsis "NASM assembly interpeter")
 (description "With some SSE instructions")
 (version 0.3.0)
 (depends
  dune
  angstrom
  (ppx_inline_test :with-test)
  ppx_expect
  ppx_deriving
  ppx_variants_conv
  bisect_ppx
  (odoc :with-doc)
  (ocamlformat :build)
  base))
