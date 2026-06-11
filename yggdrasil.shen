\\                                          Yggdrasil 2.0
\\                                    (c) Mark Tarver, 3 clause BSD
\\
\\ Tree-shaker for Shen programs, updated for ShenOSKernel 41.1.
\\
\\ Stage 1 (this file, runs on any certified Shen): shake a program against
\\ the 41.1 kernel and emit minimal KL + a manifest.  Stage 2 (per target,
\\ lives in each port repo): compile the shaken KL with the port's own
\\ KL->native compiler.
\\
\\ (yggdrasil.shake ["prog.shen"] "out") writes to out/:
\\    kernel.kl                shaken kernel defuns, in load order
\\    <prog>.kl                user code compiled to KL
\\    yggdrasil.manifest       sexp manifest
\\    yggdrasil.manifest.txt   line-oriented manifest (key=value)
\\
\\ Driver contract for builders: load kernel.kl, call (shen.initialise),
\\ then load the user files in order.  In 41.1 (shen.initialise) performs
\\ all global initialisation, so no separate globals file is needed.
\\
\\ Run from the Ygggdrasil directory: paths below are relative.

\\ No package wrapper: 41.1 has no stlib package to import from, and all
\\ stdlib functions are kernel-defined globals.  The public entry point is
\\ explicitly dot-qualified instead.

\\ ShenOSKernel-41.1 in canonical boot order (shen-cl boot.lsp order).
(set *kernel* ["KLambda/compiler.kl" "KLambda/toplevel.kl" "KLambda/core.kl"
 "KLambda/sys.kl" "KLambda/dict.kl" "KLambda/sequent.kl" "KLambda/yacc.kl"
 "KLambda/reader.kl" "KLambda/prolog.kl" "KLambda/track.kl" "KLambda/load.kl"
 "KLambda/writer.kl" "KLambda/macros.kl" "KLambda/declarations.kl"
 "KLambda/types.kl" "KLambda/t-star.kl" "KLambda/init.kl"
 "KLambda/extension-features.kl" "KLambda/extension-expand-dynamic.kl"
 "KLambda/extension-launcher.kl" "KLambda/stlib.kl"])

(set *callgraph-cache* "KLambda/callgraph-41.1.shen")

\\ The 41.1 primitives: special forms plus everything the kernel calls but
\\ does not define.  Derived mechanically: symbols in call position across
\\ KLambda/*.kl minus defun'd names.  prolog-memory, vector, variable?,
\\ read-file-as-* moved into the kernel in 41.1 and are no longer here.
(set *primitives* [if and or cond defun lambda let freeze type trap-error
      cons hd tl cons? intern pos tlstr cn str string? n->string string->n
      set value simple-error error-to-string
      absvector address-> <-address absvector?
      write-byte read-byte open close get-time eval-kl
      = + - * / > < >= <= number?
      shen.char-stinput? shen.char-stoutput?
      shen.read-unit-string shen.write-string
      *stinput* *stoutput*])

\\ ===================== self-contained list helpers ======================
\\ mapc/filter/remove-duplicates/copy-file live in 41.1's stlib, which is
\\ lazily materialised and absent from port runtimes; define our own.

(define ygg.mapc
  _ [] -> done
  F [X | Xs] -> (do (F X) (ygg.mapc F Xs)))

(define ygg.filter
  _ [] -> []
  F [X | Xs] -> [X | (ygg.filter F Xs)]  where (F X)
  F [_ | Xs] -> (ygg.filter F Xs))

(define ygg.remove-dups
  [] -> []
  [X | Xs] -> (ygg.remove-dups Xs)  where (element? X Xs)
  [X | Xs] -> [X | (ygg.remove-dups Xs)])

(define ygg.copy-file
  From To -> (let Bytes (read-file-as-bytelist From)
                  Sink  (open To out)
                  Write (ygg.mapc (/. B (write-byte B Sink)) Bytes)
                  Close (close Sink)
                  To))

\\ ============================ stage 1: shake ============================

(define yggdrasil.shake
  Files Dir -> (let MaxPrint   (value *maximum-print-sequence-size*)
                    Unlimit    (set *maximum-print-sequence-size* 1000000000)
                    Kernel     (kernel-code)
                    KLFiles    (map (fn bootstrap) Files)
                    KL         (map (fn read-file) KLFiles)
                    UserFs     (function-calls KL)
                    Foot       (footprint [shen.initialise | UserFs])
                    FootCode   (footcode Foot Kernel)
                    Prims      (find-primitives (append FootCode KL))
                    WriteK     (write-kl-file (@s Dir "/kernel.kl") FootCode)
                    UserOut    (write-user-files KLFiles KL Dir)
                    WriteM     (write-manifest Dir UserOut Prims)
                    Restore    (set *maximum-print-sequence-size* MaxPrint)
                    done))

\\ ====================== kernel call graph (cached) ======================
\\ The original Yggdrasil computed a full transitive closure with Warshall's
\\ algorithm - O(N^3) over every kernel symbol, which does not scale to the
\\ 41.1 kernel (1129 defuns, ~700K of KL).  We only ever need reachability
\\ from a seed set, so build the direct call graph once (cached to disk)
\\ and BFS over it per shake.

(define kernel-code
  -> (let Code  (mapcan (fn read-file) (value *kernel*))
          Graph (ensure-call-graph Code)
          Code))

(define ensure-call-graph
  _ -> cached  where (bound? *kernel-fns*)
  Code -> (trap-error (load-call-graph) (/. E (build-call-graph Code))))

(define load-call-graph
  -> (let Rows    (read-file (value *callgraph-cache*))
          Check   (if (empty? Rows) (error "empty call graph cache~%") loaded)
          Install (ygg.mapc (/. Row (put (hd Row) calls (tl Row))) Rows)
          (set *kernel-fns* (map (fn hd) Rows))))

(define build-call-graph
  Code -> (let Fs    (defun-names Code)
               SetFs (set *kernel-fns* Fs)
               Mark  (ygg.mapc (/. F (put F defp true)) Fs)
               Edges (ygg.mapc (fn graph-defun) Code)
               Save  (save-call-graph)
               (value *kernel-fns*)))

(define defun-names
  [] -> []
  [[defun F | _] | Code] -> [F | (defun-names Code)]
  [_ | Code] -> (defun-names Code))

(define graph-defun
  [defun F _ Body] -> (put F calls (called-fns Body))
  _ -> not-a-defun)

(define called-fns
  [X | Y] -> (union (called-fns X) (called-fns Y))
  F -> [F]   where (and (symbol? F) (kernel-defun? F))
  _ -> [])

(define kernel-defun?
  F -> (trap-error (get F defp) (/. E false)))

(define calls-of
  F -> (trap-error (get F calls) (/. E [])))

\\ Rows are written in KL paren syntax via pr-kl: read-file parses (a b c)
\\ as a plain list, whereas bracket syntax [a b c] would read back as an
\\ unevaluated (cons a (cons b ...)) AST.
(define save-call-graph
  -> (let Sink  (open (value *callgraph-cache*) out)
          Write (ygg.mapc (/. F (pr-kl-line [F | (calls-of F)] Sink))
                          (value *kernel-fns*))
          Close (close Sink)
          saved))

\\ ============================ footprint =================================

(define footprint
  Seeds -> (let Clear (ygg.mapc (/. F (put F seen false)) (value *kernel-fns*))
                (bfs Seeds [])))

(define bfs
  [] Acc -> Acc
  [F | Fs] Acc -> (bfs Fs Acc)  where (seen? F)
  [F | Fs] Acc -> (let Mark (put F seen true)
                       (bfs (append (calls-of F) Fs) [F | Acc])))

(define seen?
  F -> (trap-error (get F seen) (/. E (do (put F seen true) false))))

(define function-calls
  [X | Y] -> (union (function-calls X) (function-calls Y))
  V -> []   where (variable? V)
  F -> [F]  where (symbol? F)
  _ -> [])

(define footcode
  Footprint Kernel -> (ygg.filter (/. Def (mentioned? Def Footprint)) Kernel))

(define mentioned?
  [defun F | _] Fs -> (element? F Fs)
  _ _ -> false)

\\ ============================ primitives ================================

(define find-primitives
  X -> [X]     where (primitive? X)
  [X | Y] -> (union (find-primitives X) (find-primitives Y))
  _ -> [])

(define primitive?
  {symbol --> boolean}
  X -> (element? X (value *primitives*)))

\\ Used by backends that map primitives to copyable implementation files
\\ (the Tarver model, retained for the Lisp backend).
(define primfiles
  Primitives Language -> (ygg.remove-dups
                          (mapcan (/. Primitive (get Primitive Language)) Primitives)))

(define copy-primitive-files
  {(list string) --> string --> (list string)}
  Files Dir -> (map (/. X (copy-primitive-file X Dir)) Files))

(define copy-primitive-file
  {string --> string --> string}
  File Dir -> (let Truncate (truncate-filename File "")
                   Copy (ygg.copy-file File (@s Dir "/" Truncate))
                   Truncate))

(define truncate-filename
  {string --> string --> string}
  "" Out -> Out
  (@s "/" S) _ -> (truncate-filename S "")
  (@s S Ss) Out -> (truncate-filename Ss (cn Out S)))

\\ ========================== writing KL files ============================
\\ Shen's printer renders lists in Shen syntax ([...]), but .kl files must
\\ be in KL syntax ((...)), so we print cons trees ourselves.

(define write-kl-file
  File Code -> (let Sink  (open File out)
                    Write (ygg.mapc (/. X (do (pr-kl X Sink)
                                          (pr (make-string "~%~%") Sink))) Code)
                    Close (close Sink)
                    File))

(define pr-kl
  [] Sink -> (pr "()" Sink)
  [X | Xs] Sink -> (do (pr "(" Sink) (pr-kl X Sink) (pr-kl-body Xs Sink))
  X Sink -> (pr (make-string "~S" X) Sink))

(define pr-kl-body
  [] Sink -> (pr ")" Sink)
  [X | Xs] Sink -> (do (pr " " Sink) (pr-kl X Sink) (pr-kl-body Xs Sink)))

(define pr-kl-line
  X Sink -> (do (pr-kl X Sink) (pr (make-string "~%") Sink)))

(define write-user-files
  [] [] _ -> []
  [File | Files] [Code | Codes] Dir ->
     (let Name  (truncate-filename File "")
          Write (write-kl-file (@s Dir "/" Name) Code)
          [Name | (write-user-files Files Codes Dir)]))

\\ ============================ manifest ==================================

(define write-manifest
  Dir UserFiles Prims -> (let NeedsEval (element? eval-kl Prims)
                              Sexp (write-manifest-sexp Dir UserFiles Prims NeedsEval)
                              Txt  (write-manifest-txt Dir UserFiles Prims NeedsEval)
                              done))

(define write-manifest-sexp
  Dir UserFiles Prims NeedsEval ->
    (let Sink (open (@s Dir "/yggdrasil.manifest") out)
         W1 (pr-kl-line ["yggdrasil-manifest" 1] Sink)
         W2 (pr-kl-line ["kernel-version" "41.1"] Sink)
         W3 (pr-kl-line ["kernel" "kernel.kl"] Sink)
         W4 (pr-kl-line ["init" shen.initialise] Sink)
         W5 (pr-kl-line ["user" | UserFiles] Sink)
         W6 (pr-kl-line ["primitives" | Prims] Sink)
         W7 (pr-kl-line ["needs-eval" NeedsEval] Sink)
         (close Sink)))

(define write-manifest-txt
  Dir UserFiles Prims NeedsEval ->
    (let Sink (open (@s Dir "/yggdrasil.manifest.txt") out)
         W1 (pr (make-string "manifest-version=1~%") Sink)
         W2 (pr (make-string "kernel-version=41.1~%") Sink)
         W3 (pr (make-string "kernel=kernel.kl~%") Sink)
         W4 (pr (make-string "init=shen.initialise~%") Sink)
         W5 (ygg.mapc (/. F (pr (make-string "user=~A~%" F) Sink)) UserFiles)
         W6 (ygg.mapc (/. P (pr (make-string "primitive=~A~%" P) Sink)) Prims)
         W7 (pr (make-string "needs-eval=~A~%" NeedsEval) Sink)
         (close Sink)))

