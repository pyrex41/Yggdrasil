\\                                          Yggdrasil 1.0 
\\                                    (c) Mark Tarver, 3 clause BSD


(package yggdrasil (append (external stlib) [yggdrasil compiler cl.kl-to-lisp done boilerplate driver])

\\(cd "C:\Users\shend\OneDrive\Desktop\Shen\Yggdrasil")
 
(set *kernel* ["KLambda\sequent.kl" "KLambda\sys.kl" "KLambda\toplevel.kl"
"KLambda\track.kl" "KLambda\t-star.kl" "KLambda\types.kl" "KLambda\writer.kl"
"KLambda\yacc.kl" "KLambda\backend.kl" "KLambda\core.kl" "KLambda\declarations.kl"
"KLambda\load.kl" "KLambda\macros.kl" "KLambda\prolog.kl" "KLambda\reader.kl"
"KLambda\complex.dtype.kl" "KLambda\complex.kl" "KLambda\data.kl"
"KLambda\date.kl" "KLambda\encrypt.kl" "KLambda\files.kl"
"KLambda\lists.kl" "KLambda\maths.kl" "KLambda\numerals.dtype.kl"
"KLambda\numerals.kl" "KLambda\prettyprint.kl" "KLambda\rationals.dtype.kl"
"KLambda\rationals.kl" "KLambda\smart.kl" "KLambda\strings.kl"
"KLambda\symbols1.kl" "KLambda\symbols2.kl" "KLambda\tuples.kl"
"KLambda\vectors.kl"]) 
   
(define yggdrasil
   Language Files Dir -> (let  FDG      (if (bound? *ttable*)
                                        (value *ttable*)
                                        (fdg))
                               Compiler     (get Language compiler)             
                               KLFiles      (map (fn bootstrap) Files)
                               KL           (map (fn read-file) KLFiles)
                               UserFs       (function-calls KL)
                               Footprint    (footprint UserFs)
                               Kernel       (mapcan (fn read-file) (value *kernel*))
                               FootCode     (footcode Footprint Kernel)
                               Globals      (find-globals FootCode)
                               GlobalCode   (global-code Globals)                                
                               Primitives   (find-primitives (append FootCode GlobalCode KL))
                               PrimFiles    (primfiles Primitives Language)
                              \\ PR           (print 1)                                                             
                               CopyPrim     (copy-primitive-files PrimFiles Dir)
                               \\ PR           (print 2)
                               Sink         (open (@s Dir "/boilerplate." Language) out)
                               Boilerplate  (pr (get Language boilerplate) Sink)
                               Close        (close Sink)  
                              \\ PR           (print 3)
                               Sink         (open (@s Dir "/globals." Language) out)
                               GlobalOb     (mapc (/. X (pr (Compiler X) Sink)) GlobalCode) 
                               Close        (close Sink)
                              \\ PR           (print 4)
                               Sink         (open (@s Dir "/kernel." Language) out)
                               ObKernel     (mapc (/. X (pr (Compiler X) Sink)) FootCode)
                              \\ PR           (print 5)
                               Close        (close Sink)
                               OBUser       (obuser-files KLFiles KL Language Compiler Dir)                               
                               \\PR           (print 6)
                               Sink         (open (@s Dir "/driver." Language) out)
                               DriverCode   ((get Language driver) (cn "boilerplate." Language)
                                                                   CopyPrim
                                                                   (cn "globals." Language)
                                                                   (cn "kernel." Language)
                                                                 OBUser)
                               \\PR           (print 7)                                    
                               Driver       (pr DriverCode Sink)                                    
                               Close        (close Sink)
                               \\PR           (print 8)
                               done))
                               
(define copy-primitive-files
  {(list string) --> string --> (list string)}
  Files Dir -> (map (/. X (copy-primitive-file X Dir)) Files))
  
(define copy-primitive-file
  {string --> string --> string}
  File Dir -> (let Truncate (truncate-filename File "")
                   Copy (copy-file File (@s Dir "/" Truncate))
                   Truncate))
                   
(define truncate-filename
  {string --> string --> string}
  "" Out -> Out
  (@s "/" S) _ -> (truncate-filename S "")
  (@s S Ss) Out -> (truncate-filename Ss (cn Out S)))                               
                           
(define find-globals
  X -> [X]     where (bound? X)
  [X | Y] -> (union (find-globals X) (find-globals Y))
  _ -> [])
  
(define find-primitives
  X -> [X]     where (primitive? X)
  [X | Y] -> (union (find-primitives X) (find-primitives Y))
  _ -> [])
  
(define primfiles
  Primitives Language -> (remove-duplicates (mapcan (/. Primitive (get Primitive Language)) Primitives)))
    
(define primitive?
  {symbol --> boolean}
  X -> (element? X (value *primitives*)))
  
(set *primitives* [if and or cond intern prolog-memory vector
      pos tlstr cn str string? n->string string->n
      set value simple-error trap-error error-to-string
      cons hd tl cons? absvector address-> <-address absvector?
      write-byte read-byte open close + - * / > < >= <= number?
      defun lambda let = eval-kl freeze type get-time *stinput* 
      *stoutput* shen.char-stinput? shen.char-stoutput? 
			shen.write-string shen.read-unit-string set])  
  
(define global-code
  {(list symbol) --> (list s-expr)}
  Globals -> (mapcan (fn global-assignment) Globals))  
  
(define global-assignment
  {symbol --> s-expr}
  shen.*history*         -> [[set shen.*history* []]]
  shen.*tc*              -> [[set shen.*tc* false]]
  *property-vector*      -> [[set *property-vector* [vector 20000]]]
  shen.*gensym*          -> [[set shen.*gensym* 0]]
  shen.*tracking*        -> [[set shen.*tracking* []]]
  shen.*profiled*        -> [[set shen.*profiled* []]]
  *home-directory*       -> [[set *home-directory* ""]]
  shen.*special*         -> [[set shen.*special* [cons @p [cons @s [cons @v 
                                                  [cons cons [cons lambda [cons let 
                                                    [cons where [cons set [cons open 
                                                      [cons input+ [cons type [] ]]]]]]]]]]]]]
  shen.*extraspecial*    -> [[set shen.*extraspecial* []]]  
  shen.*spy*             -> [[set shen.*spy* false]]
  shen.*datatypes*       -> [[set shen.*datatypes* []]]
  shen.*alldatatypes*    -> [[set shen.*alldatatypes* []]]
  shen.*shen-type-theory-enabled?* -> [[set shen.*shen-type-theory-enabled?* true]]
  shen.*package*         -> [[set shen.*package* null]]
  shen.*synonyms*        -> [[set shen.*synonyms* []]]
  shen.*system*          -> [[set shen.*system* []]]
  shen.*sigf*            -> [[set shen.*sigf* []]]
  shen.*occurs*          -> [[set shen.*occurs* true]]
  shen.*factorise?*      -> [[set shen.*factorise?* false]]
  shen.*maxinferences*   -> [[set shen.*maxinferences* 1000000]]
  *maximum-print-sequence-size* -> [[set *maximum-print-sequence-size* 20]]
  shen.*call*            -> [[set shen.*call* 0]]
  shen.*infs*            -> [[set shen.*infs* 0]]
  *hush*                 -> [[set *hush* false]]
  shen.*optimise*        -> [[set shen.*optimise* false]]
  *version*              -> [[set *version* "34.6"]]
  shen.*step*            -> [[set shen.*step* false]]
  shen.*it*              -> [[set shen.*it* ""]]
  shen.*residue*         -> [[set shen.*residue* []]]
  *stoutput*             -> []
  *stinput*              -> []
  *macros*               -> [[set *macros* []]]
  shen.*prolog-vector*   -> [[prolog-memory 1e4]]
  /                      -> [] 
  *                      -> []
  +                      -> []
  -                      -> []
  shen.*lambdatable*     -> [[set shen.*lambdatable* []]]
  shen.*loading?*        -> [[set shen.*loading?* false]]  )                      
                           
(define obuser-files
   [] [] _ _ _ -> []
   [File | Files] [KL | KLs] Language Compiler Dir -> (let ObFile (file-extension File (cn "." Language))
                                                           Sink (open (@s Dir "/" ObFile) out)
                                                           Obcode (map Compiler KL)
                                                           Write  (mapc (/. X (pr X Sink)) Obcode)
                                                           Close (close Sink)
                                                          [ObFile | (obuser-files Files KLs Language Compiler Dir)]))                             
                 
(define function-calls
  [X | Y] -> (union (function-calls X) (function-calls Y))
  V -> []   where (variable? V)
  F -> [F]    where (symbol? F)
  _ -> []) 
  
(define footprint
  {(list symbol) --> (list symbol)}
  UserFs -> (mapcan (/. F (assoc F (value *ttable*))) UserFs))   
  
(define footcode
  Footprint Kernel -> (filter (/. Def (mentioned? Def Footprint)) Kernel)) 
                 
(define mentioned?
  [defun F | _] Fs -> (element? F Fs)
  _ _ -> false)                 
   
(define fdg
  {--> (list (list symbol))}
        -> (let Files (value *kernel*)
             \\   MaxPrint (value *maximum-print-sequence-size*)
             \\   SetMaxPrint (set *maximum-print-sequence-size* 1e5)
                Code   (mapcan (fn read-file) Files)
                Fs     (extract-Fs Code)
                Matrix (warshall Fs (/. F1 F2 (calls? F1 F2 Code)))
                TTable (set *ttable* [])
                ComputeTTable (compute-fdg-from-matrix Fs Matrix)
               \\ Write (write-table-to-file)
             \\   ResetMaxPrint (set *maximum-print-sequence-size* MaxPrint)
                (value *ttable*)))
                
\\(define write-table-to-file
 \\ {--> (list A)}
 \\ -> (let Sink (open "ttrans.shen" out)
   \\       Write (mapc (/. X (pr (make-string "~A~%" X) Sink)) (value *ttable*))
      \\    (close Sink)))                
                
(define compute-fdg-from-matrix
  {(list symbol) --> (vector (vector boolean)) --> (list (list symbol))}
   Fs Matrix -> (let N (length Fs)
                   (for X = 1 (<= X N)
                      (for Y = 1 (<= Y N)
                         (if (:= Matrix [X Y])
                             (assoc-add (nth X Fs) (nth Y Fs))
                             [])))))
                             
(define assoc-add
  {symbol --> symbol --> (list (list symbol))}
   Caller Called -> (set *ttable* (assoc-add-h Caller Called (value *ttable*))))
   
(define assoc-add-h
  {A --> A --> (list (list A)) --> (list (list A))}
   Caller Called [] -> [[Caller Called]]
   Caller Called [[Caller | Calls] | Table] -> [[Caller Called | Calls] | Table]
   Caller Called [Entry | Table] -> [Entry | (assoc-add-h Caller Called Table)])                                            
                
(define calls?
  F1 F2 Code -> (let DefF1 (def F1 Code)
                     (> (occurrences F2 DefF1) 0)))
                     
(define extract-Fs
  [X | Y] -> (union (extract-Fs X) (extract-Fs Y))
  X -> []    where (variable? X)
  X -> [X]   where (symbol? X)
  _ -> [])                     
                     
(define def
  _ [] -> []
  F1 [[defun F1 _ Body] | _] -> Body
  F1 [[set F1 Body] | _] -> Body
  F1 [_ | Defs] -> (def F1 Defs))                                     
  
(define warshall
  {(list A) --> (A --> A --> boolean) --> (vector (vector boolean))}
    L R? -> (let N        (length L)
                 Matrix   (array [N N])
                 Populate (for X = 1 (<= X N)
                            (for Y = 1 (<= Y N)
                             (Matrix [X Y] := (R? (nth X L) (nth Y L)))))
                 Warshall (iterate-warshall N Matrix)
                 Matrix))
                 
(define iterate-warshall 
  {number --> (vector (vector boolean)) --> (vector (vector boolean))}
   N Matrix -> (for J = 1 (<= J N)
                  (for I = 1 (<= I N)
                    (if (and (not (= I J)) (:= Matrix [I J]))
                       (for K = 1 (<= K N)
                         (Matrix [I K] := (or (:= Matrix [I K]) (:= Matrix [J K]))))
                       Matrix)))) )