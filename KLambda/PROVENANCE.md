# Kernel provenance

The `.kl` files in this directory are the **ShenOSKernel-41.2** KLambda
sources, vendored byte-for-byte from the official release:

- **Release:** <https://github.com/Shen-Language/shen-sources/releases/tag/shen-41.2>
- **Asset:** `ShenOSKernel-41.2.zip`
- **SHA-256:** `49f1b85d02348d9b3ebc461570c5c56cc066270ab81e35d5257625fb9d17fe82`

Exceptions:

- `compiler.kl` is **not part of the release** — it is shen-cl's KL→Lisp
  compiler (generated from `shen-cl/src/compiler.shen` by `make
  precompile`) and must match the shen-cl binary used to run the shaker.
- `callgraph-41.2.shen` is a generated cache (gitignored).

Note that the ShenOSKernel releases are the community packaging of the
kernel (Shen-Language/shen-sources) and differ deliberately from Mark
Tarver's S-series distributions on shenlanguage.org (tracked at
pyrex41/shen-s41.1); all Ratatoskr ports certify against the
ShenOSKernel packaging.
