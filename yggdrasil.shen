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
                    Graph      (call-graph Kernel)
                    KLFiles    (map (fn bootstrap) Files)
                    KL         (map (fn read-file) KLFiles)
                    UserFs     (function-calls KL)
                    Foot       (footprint [shen.initialise | UserFs] Graph)
                    FootCode   (map (/. D (trim-lambda-forms D Foot))
                                    (footcode Foot Kernel))
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
\\ and run a worklist traversal over it per shake.  Full rationale,
\\ including why a faster external closure (Julia/bitsets) is still the
\\ wrong tool: docs/reachability.md.
\\
\\ The graph is a VALUE: a list of rows [F | Callees] threaded through the
\\ footprint computation.  The cache file is plain text - one row per line,
\\ space-separated names - parsed with string primitives.  It must NOT go
\\ through read-file: the Shen reader applies the currying transform to
\\ paren applications (and turns bracket lists into cons ASTs), silently
\\ corrupting any row whose head's declared arity differs from its length.

(define kernel-code
  -> (mapcan (fn read-file) (value *kernel*)))

(define call-graph
  Code -> (trap-error (load-call-graph) (/. E (build-call-graph Code))))

(define load-call-graph
  -> (let Bytes (read-file-as-bytelist (value *callgraph-cache*))
          Rows  (parse-graph Bytes "" [] [])
          (if (empty? Rows) (error "empty call graph cache~%") Rows)))

\\ parse-graph Bytes Token Row Rows: accumulate chars into Token, tokens
\\ into Row, rows into Rows.  Walks the bytelist (O(n)); recursing over a
\\ string with @s patterns would copy the tail each step (O(n^2)).
(define parse-graph
  [] Token Row Rows -> (reverse (close-row Token Row Rows))
  [10 | Bs] Token Row Rows -> (parse-graph Bs "" [] (close-row Token Row Rows))
  [13 | Bs] Token Row Rows -> (parse-graph Bs Token Row Rows)
  [32 | Bs] Token Row Rows -> (parse-graph Bs "" (close-token Token Row) Rows)
  [B | Bs] Token Row Rows -> (parse-graph Bs (cn Token (n->string B)) Row Rows))

(define close-token
  "" Row -> Row
  Token Row -> [(intern Token) | Row])

(define close-row
  Token Row Rows -> (let Full (close-token Token Row)
                         (if (empty? Full) Rows [(reverse Full) | Rows])))

(define build-call-graph
  Code -> (let Fs    (defun-names Code)
               Mark  (ygg.mapc (/. F (put F defp true)) Fs)
               Graph (graph-rows Code)
               Save  (save-call-graph Graph)
               Graph))

(define defun-names
  [] -> []
  [[defun F | _] | Code] -> [F | (defun-names Code)]
  [_ | Code] -> (defun-names Code))

(define graph-rows
  [] -> []
  [[defun F _ Body] | Code] -> [[F | (called-fns Body)] | (graph-rows Code)]
  [_ | Code] -> (graph-rows Code))

\\ Two kernel data tables masquerade as code and would otherwise drag
\\ ~every public symbol into every footprint:
\\   - the arity table literal is pure name/number data;
\\   - lambda-form entries (cons F (lambda Y (F Y))) are eta-wrappers
\\     whose only callee is their own key F.  We drop their edges here
\\     and instead filter the entries to the footprint at write time
\\     (see trim-lambda-forms), so a kept entry's F is in Foot already.
(define called-fns
  [shen.initialise-arity-table _] -> [shen.initialise-arity-table]
  [shen.set-lambda-form-entry [cons _ _]] -> [shen.set-lambda-form-entry]
  [put P shen.external-symbols _ | Rest] -> (union (called-fns put) (called-fns Rest))
      where (symbol? P)
  [set shen.*special* _] -> (called-fns set)
  [set shen.*extraspecial* _] -> (called-fns set)
  [shen.assoc-> K | R] -> (union (called-fns shen.assoc->) (called-fns R))
      where (symbol? K)
  [X | Y] -> (union (called-fns X) (called-fns Y))
  F -> [F]   where (and (symbol? F) (kernel-defun? F))
  _ -> [])

\\ defp is a build-time-only membership test: called-fns visits every
\\ symbol leaf of ~700K of KL, where (element? F Fs) over 1129 names
\\ would cost ~45M comparisons.  Never consulted per-shake.
(define kernel-defun?
  F -> (trap-error (get F defp) (/. E false)))

(define save-call-graph
  Graph -> (let Sink  (open (value *callgraph-cache*) out)
                Write (ygg.mapc (/. Row (pr-graph-row Row Sink)) Graph)
                Close (close Sink)
                saved))

(define pr-graph-row
  [F | Calls] Sink -> (do (pr (str F) Sink)
                          (ygg.mapc (/. C (pr (cn " " (str C)) Sink)) Calls)
                          (pr (n->string 10) Sink)))

\\ ============================ footprint =================================
\\ Pure worklist reachability: the visited set is the accumulator itself.
\\ Seeds that are not kernel functions fall through row-calls to [].

(define footprint
  Seeds Graph -> (reach Seeds [] Graph))

(define reach
  [] Seen _ -> Seen
  [F | Fs] Seen Graph -> (reach Fs Seen Graph)    where (element? F Seen)
  [F | Fs] Seen Graph -> (reach (append (row-calls F Graph) Fs)
                                [F | Seen] Graph))

(define row-calls
  F [[F | Calls] | _] -> Calls
  F [_ | Rows] -> (row-calls F Rows)
  _ [] -> [])

(define function-calls
  [X | Y] -> (union (function-calls X) (function-calls Y))
  V -> []   where (variable? V)
  F -> [F]  where (symbol? F)
  _ -> [])

(define footcode
  Footprint Kernel -> (ygg.filter (/. Def (mentioned? Def Footprint)) Kernel))

\\ Rewrite shen.initialise-lambda-forms to register eta-wrappers only for
\\ footprint functions (its do-chain is right-nested KL).  Counterpart of
\\ the called-fns special case above.
(define trim-lambda-forms
  [defun shen.initialise-lambda-forms P Body] Foot ->
      [defun shen.initialise-lambda-forms P (trim-lf-chain Body Foot)]
  Def _ -> Def)

(define trim-lf-chain
  [do E Rest] Foot -> (let R (trim-lf-chain Rest Foot)
                           (if (lf-keep? E Foot) [do E R] R))
  E Foot -> (if (lf-keep? E Foot) E true))

(define lf-keep?
  [shen.set-lambda-form-entry [cons F _]] Foot -> (element? F Foot)
  _ _ -> true)

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

