/*

Timing:
- 10 minutes for presentation
- 10 minutes for Q/A

Proposed structure:
1. [--] Title slide (your name, and the name of your supervisor)
2. [4m] Context and related works
  - context: what is the area are you working on, what are the open problems in this area (that motivate your work)?
  - related works: what has already been done in the area that your are building on or comparing against?
3. [1m] Problem statement — what exactly are you trying to solve/achieve in your work?
4. [4m] Solution — your proposed solution, techniques used, evaluation — how do you approach the problem, which techniques are crucial, how well does your approach work? Mention only core ideas, you do not have time to go into any details.
5. [1m] Results & future work — finalize, clearly indicate your contribution, mention limitations and directions of future work. You will stop your presentation here!
6. [--] References + extra slides

Remember:
1. Avoid writing too much text on the slides (use presenter's notes for that), the slides should contain key points.
2. Avoid writing too little text on the slides (we still need to have proper amount of information).
3. Cite your sources (where did you get the information/picture/diagram/etc.).
4. Provide links to things you made (GitHub repo, pull requests, other artifacts).
5. Mention people you work with!
6. If you have presented your work in any workshops/conferences/seminars — mention it in the results!

Recommendations / Hints:
- Be ready to justify each statement in the presentation.
- One common code example (сквозной пример) used throughout the presentation — a good way to explain different conecpts.
- "Predict" possible questions and prepare extra slides at the end, so that you can easily answer them.
*/
#import "lib/mahpar.typ": mathpar
#import "@preview/simplebnf:0.1.1" as bnf
#import "@preview/curryst:0.5.1": rule, prooftree

#set page(
  width: 1920pt,
  height: 1080pt,
)

#set text(size: 48pt)

#show smallcaps: set text(font: "Latin Modern Roman Caps")

#show heading.where(level: 1): it => [
  #set text(size: 64pt, weight: 700)
  #block(below: 1.2em, it)
]

// Used to toggle code blocks width between auto/100%
#let rawFull = state("raw-full", false)

#show raw: set text(font: ("Geist Mono", "DejaVu Sans Mono"))
#show raw.where(block: true): it => context [
  #set text(size: 32pt)
  #if rawFull.get() {
    block(
      width: 100%,
      fill: luma(95%),
      inset: 24pt,
      radius: 10pt,
      it
    )
  } else {
    block(
      width: auto,
      fill: luma(95%),
      inset: 24pt,
      radius: 10pt,
      it
    )
  }
]

#let slide(body, footer: "") = {
  page(
    body,
    numbering: "1",
    footer: context [
      #set text(fill: luma(65%), size: 42pt)
      #footer
      #h(1fr)
      #counter(page).display("1 / 1", both: true)
    ]
  )
}

////////////////////////////////////////////////////////////////////////////////
// Slides
////////////////////////////////////////////////////////////////////////////////
#page(
  footer: [
    #align(center)[
      #text(size: 42pt)[Innopolis, 2025]
    ]
  ]
)[
  #align(center+horizon)[
    #text(size: 72pt, weight: "bold")[
      Hindley-Milner-style Type Inference with Levels\
      for Generic Abstract Syntax with Binders
    ]
    #block[
      #set align(left)
      #text(size: 42pt, fill: luma(30%))[
        Author: Vladislav Deryabkin\
        Supervisor: Nikolai Kudasov
      ]
    ]
  ]
]
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Context

  Type system is a crucial part of any formal system (e.g. programming language).

  Implementing one is usually hard due to following challanges:

  + How to handle bound names and avoid capturing?
  + How to make implementation efficient and extendable?
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#show math.equation: set text(size: 64pt)
#slide([
  = Hindley-Milner Type System

  Foundation for functional programming languages (e.g., Haskell, ML, OCaml).
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Hindley-Milner Type System

  Foundation for functional programming languages (e.g., Haskell, ML, OCaml).

  Main properties:
  - *Inference of the most general (principal) type without annotations;*

  #align(center+horizon)[#rect(inset: 42pt)[
    $lambda x. x : forall alpha. alpha arrow alpha$
  ]]
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Hindley-Milner Type System

  Foundation for functional programming languages (e.g., Haskell, ML, OCaml).

  Main properties:
  - Inference of the most general (principal) type without annotations;
  - *Support of parametric polymorphism;*

  #align(center+horizon)[#rect(inset: 42pt)[
    $#raw("let") "id" = lambda x. x #raw("in") dots" "("id" 3)" "dots" "("id" "\"text\"")$
  ]]
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Hindley-Milner Type System

  Foundation for functional programming languages (e.g., Haskell, ML, OCaml).

  Main properties:
  - Inference of the most general (principal) type without annotations;
  - Support of parametric polymorphism;
  - *Soundness and completeness have been proved @Damas1984_TypeAssignment.*
], footer: "Context & Related Works")
// TODO: Be able to explain what does it mean "sound" and "complete"?
////////////////////////////////////////////////////////////////////////////////

#let prooftree = prooftree.with(min-premise-spacing: 52pt)

#let hm_abs = prooftree(
  rule(
    name: $[#smallcaps("Abs")]$,
    $Gamma tack.r lambda x. e : tau arrow.r tau'$,
    $Gamma, x : tau tack.r e : tau'$
  )
)
#let hm_app = prooftree(
  rule(
    name: $[#smallcaps("App")]$,
    $Gamma tack.r e_0" "e_1 : tau'$,
    $Gamma tack.r e_0 : tau arrow.r tau'$,
    $Gamma tack.r e_1 : tau$
  )
)
#let hm_var = prooftree(
  rule(
    name: $[#smallcaps("Var")]$,
    $Gamma tack.r x : tau$,
    $x : sigma in Gamma$,
    $sigma subset.eq.sq tau$
  )
)
#let hm_let = prooftree(
  rule( 
    name: $[#smallcaps("Let")]$,
    $Gamma tack.r #raw("let") x = e_0 #raw("in") e_1 : tau'$,
    $Gamma tack.r e_0 : tau$,
    $Gamma, x : overline(Gamma)(tau) tack.r e_1 : tau'$
  )
)
#let hm_let_red = prooftree(
  rule( 
    name: $[#smallcaps("Let")]$,
    $Gamma tack.r #raw("let") x = e_0 #raw("in") e_1 : tau'$,
    $Gamma tack.r e_0 : tau$,
    $Gamma, x : #[#set text(fill: red);$overline(Gamma)(tau)$] tack.r e_1 : tau'$
  )
)
#let hm_g1 = $overline(Gamma)(tau) = forall hat(alpha).tau$
#let hm_g1_red = $overline(Gamma)(tau) = forall #[#set text(fill: red);$hat(alpha)$] . tau$
#let hm_g2 = $hat(alpha) = text("free")(tau) \\ text("free")(Gamma)$
#let hm_g2_red = $hat(alpha) = text("free")(tau) #[#set text(fill: red);$\\ text("free")(Gamma)$]$
#set rect(inset: 32pt)

#show math.equation: set text(size: 46pt)

#slide([
  = Let-polymorphism in HM

  #rect(mathpar(
    row-gutter: 96pt,
    hm_abs,
    hm_app,
    hm_let,
    hm_var
  ))

  #rect(mathpar(hm_g1, hm_g2))
], footer: "Context & Related Works")

#slide([
  = Let-polymorphism in HM

  #set text(fill: gray)
  #set rect(stroke: gray)

  #rect(mathpar(
    row-gutter: 96pt,
    hm_abs,
    hm_app,
    [
      #set text(fill: blue)
      #hm_let
    ],
    [
      #set text(fill: maroon)
      #hm_var
    ],
  ))

  #[
    #set text(fill: blue)
    #set rect(stroke: blue)
    #rect(mathpar(hm_g1, hm_g2))
  ]

  #place(
    top + left,
    dx: 8%,
    dy: 34%,
    rotate(-5deg)[#text(fill: blue, weight: "bold", "generalization")],
  )

  #place(
    top + right,
    dx: -8%,
    dy: 34%,
    rotate(5deg)[#text(fill: maroon, weight: "bold", "specialization")],
  )
], footer: "Context & Related Works")


#slide([
  = Let-polymorphism in HM

  #set text(fill: luma(100))
  #set rect(stroke: luma(100))

  #rect(mathpar(
    row-gutter: 96pt,
    hm_abs,
    hm_app,
    hm_let_red,
    hm_var,
  ))

  #rect(mathpar(hm_g1_red, hm_g2_red))

  #set text(fill: black)

  Time complexity of computing $overline(Gamma)(tau)$ depends on size of $Gamma$: $O(|"free"(tau)| #{text(fill: red, $+ |"free"(Gamma)|$)})$.
], footer: "Context & Related Works")

////////////////////////////////////////////////////////////////////////////////
#slide([
  = Rémy ranks @Remy1992_SortedEqTheoryTypes (levels)

  + Each free type variable is assigned with a rank (let-nesting level);
  + Level of a variable may be updated during unification;
  + Only variables with level $>=$ current level are generalized.

  #align(center+horizon)[
    ```
    λa.                         -- a level is 0
      let x = (λb.              -- b level is 1
        let y = (λc.            -- c level is 2
          let z = (λd. a b c d) -- d level is 3
            in z) in y) in x)
    ```
  ]

  Time complexity of generalizing a type variable via levels is $O(|"free"(tau)|)$.
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Name management

  1. *How to handle bound identifiers avoiding name capture?*

  #align(center)[#rect(inset: 42pt)[
    $(lambda x. lambda y. x) space y #h(48pt) attach(arrow.r, br: beta) #h(48pt) [x arrow.bar y](lambda y. x) #h(48pt) #text(fill: red, $equiv.not #h(48pt) lambda y. y$)$
  ]]
], footer: "Context & Related Works")
#slide([
  = Name management

  1. How to handle bound identifiers avoiding name capture?
  2. *How to represent AST in code?*
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Existing approaches

  1. Rapier (2002) — fast and parallelizable technique employed in GHC @Simon2002_SecretsGHC
  2. Foil (2022) — scope-safe rapier implementation by Google Research @Foil
  3. Free Foil (2024) — framework that generates scope-safe generic SOAS #footnote("Second-Order Abstract Syntax") @FreeFoil
    - Based on Foil — scope-safety
    - Based on data types à la carte @Swierstra2008_a_la_carte — generic
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = SOAS via Free Foil

  `AST` type from Free Foil is foundation for the SOAS:

  ```hs
  -- Represents terms in a generated AST based on signature 'sig',
  -- where 'binder' is type of binders and 'n' is phantom variable representing scope.
  data AST binder sig n where
    Var :: Name n -> AST binder sig n
    Node :: sig (ScopedAST binder sig n) (AST binder sig n) -> AST binder sig n

  -- Scoped version of a term with bound construct and subterm that may use it.
  data ScopedAST binder sig n where
    ScopedAST :: binder n l -> AST binder sig l -> ScopedAST binder sig n
  ```
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Goals

  + Design a Hindley-Milner-style inference algorithm with generalization via levels;
  + Implement the algorithm in Haskell with scope-safe generic abstract syntax;
  + Develop a test-suite to verify correctness;
  + Generalize implementation to make it reusable.
], footer: "Problem Statement")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Object language grammar

  #show emph: it => text(fill: luma(120), it)

  #bnf.bnf(
    bnf.Prod(
      $e$,
      annot: $sans("Expr")$,
      {
        bnf.Or[$0 | 1 | dots$][_constant natural number_]
        bnf.Or[#raw("true")][_constant true_]
        bnf.Or[#raw("false")][_constant false_]
        bnf.Or[$x$][_variable_]
        bnf.Or[$lambda x. e$][_abstraction_]
        bnf.Or[$e space e$][_application_]
        bnf.Or[$#raw("let") x = e #raw("in") e$][_let-binding_]
      },
    ),
    bnf.Prod(
      $tau$,
      annot: $sans("Type")$,
      {
        bnf.Or[#raw("Nat")][_natural number type_]
        bnf.Or[#raw("Bool")][_boolean type_]
        bnf.Or[$alpha$][_type variable_]
        bnf.Or[$tau arrow.r tau$][_function (arrow)_]
        bnf.Or[$forall alpha. tau$][_forall_]
      },
    ),
  )
], footer: "Proposed Solution")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Algorithm L

  #align(center)[#rect()[
    $
      cal("L") &:: T times sans("Expr") arrow T times sans("Type")
    $
  ]]

  $T = (C, cal("S"), Gamma, M, cal("l"))$ is a typing context with 5 components: #h(1fr) Examples:

  + $C$ — set of constraints to be resolved #h(1fr) ${(tau_1 equiv tau_2 arrow #raw("Bool")), (tau_2 equiv #raw("Nat"))}$

  + $cal("S")$ — accumulated substitution #h(1fr) ${(tau_2 arrow.bar #raw("Nat"))}$

  + $Gamma$ — typing environment #h(1fr) ${(x : #raw("Nat")), (y : tau_1)}$

  + $M$ — level map of type variables #h(1fr) ${(tau_1 arrow.bar 0), (tau_1 arrow.bar 2)}$

  + $cal("l")$ — current level #h(1fr) 5
], footer: "Proposed Solution")
#slide([
  = Algorithm L (2) — auxiliary functions

  Algorithm $cal("L")$ relies on 3 auxiliary functions:

  #align(center)[#rect()[
    $
      serif("gen") &:: sans("Type") times M times bb("N") arrow sans("Type") \
      serif("spec") &:: sans("Type") arrow sans("Type") times A^* \
      serif("unify") &:: C times cal("S") times M arrow cal("S") times M
    $
  ]]

  - $serif("gen")(tau, m, l)$ — generalizes a monotype $tau$ with a given level map $m$ at level $l$
    - does not depend on $Gamma$
  - $serif("spec")(sigma)$ — specializes a polytype $sigma$ by instantiating all $forall$-bound variables
  - $serif("unify")(c, s, m)$ — resolves all constraints $s$, returning composed substitution $s$
    - updates the level map $m$ when types at different levels are unified
], footer: "Proposed Solution")
#show math.equation: set text(size: 52pt)
#slide([
  = Algorithm L (3) — inference

  - Literals:

  #align(center)[#rect()[
    $
      &cal("L")(T_1, n) &= &(T_1, #raw("Nat")), "where" n in bb("N") \
      &cal("L")(T_1, #raw("true")) &= &(T_1, #raw("Bool")) \
      &cal("L")(T_1, #raw("false")) &= &(T_1, #raw("Bool")) \
    $
  ]]
], footer: "Proposed Solution")
#slide([
  = Algorithm L (4) — inference

  - Variable:

  #align(center)[#rect()[
    $
      // Variable
      &cal("L")((C_1, cal("S")_1, Gamma_1, M_1, cal("l")_1), x) &&= & #[*let*] & (tau_1, hat(alpha)) = serif("spec")(tau_0), "where" (x:tau_0) in Gamma_1 \
      &&&&&M_2 = M_1 union {alpha arrow.bar cal("l")_1 | alpha in hat(alpha)} \
      &&&&#[*in*] &((C_1, cal("S")_1, Gamma_1, M_2, cal("l")_1), tau_1)
    $
  ]]
], footer: "Proposed Solution")
#slide([
  = Algorithm L (5) — inference

  - Abstraction:

  #align(center)[#rect()[
    $
      // Abstraction
      &cal("L")((C_1, cal("S")_1, Gamma_1, M_1, cal("l")_1), lambda x. e) &&= & #[*let*] & Gamma_2 = Gamma_1 union {(x : beta)}, beta "is fresh" \
      &&&&&M_2 = M_1 union {(beta arrow.bar cal("l")_1)} \
      &&&&&(T_3, tau_0) = cal("L")((C_1, cal("S")_1, Gamma_2, M_2, cal("l")_1), e) \
      &&&&#[*in*] &(T_3, beta arrow t_0)
    $
  ]]
], footer: "Proposed Solution")
#slide([
  = Algorithm L (6) — inference

  - Application:

  #align(center)[#rect()[
    $
      // Application
      &cal("L")(T_1, e_1 space e_2) &&= & #[*let*] & (T_2, tau_1) = cal("L")(T_1, e_1) \
      &&&&&((C_3, cal("S")_3, Gamma_3, M_3, cal("l")_3), tau_2) = cal("L")(T_2, e_2) \
      &&&&&C_4 = C_3 union {(tau_1 equiv tau_2 arrow beta)}, beta "is fresh" \
      &&&&&M_4 = M_3 union {(beta arrow.bar cal("l")_3)} \
      &&&&#[*in*] & ((C_4, cal("S")_3, Gamma_3, M_4, cal("l")_3), beta)
    $
  ]]
], footer: "Proposed Solution")
#slide([
  = Algorithm L (7) — inference

  - Let-binding:

  #align(center)[#rect()[
    $
      // Application
      &cal("L")(T_1, #[`let`] x = e_1 #[`in`] e_2) &&= & #[*let*] &(C_1, cal("S")_1, Gamma_1, M_1, cal("l")_1) = T_1 \
      &&&&&(T_2, tau_1) = cal("L")((C_1, cal("S")_1, Gamma_1, M_1, cal("l")_1 + 1), e_1) \
      &&&&&(C_2, cal("S")_2, Gamma_2, M_2, cal("l")_2) = T_2 \
      &&&&&(cal("S")_3, M_3) = serif("unify")(C_2, cal("S")_2, M_2) \
      &&&&&Gamma_3 = cal("S")_3 Gamma_3 union {(x : serif("gen")(cal("S")_3tau_1, M_3, cal("l")_1))}\
      &&&&#[*in*] & cal("L") ((diameter, cal("S")_3, Gamma_3, M_3, cal("l")_1), e_2)
    $
  ]]
], footer: "Proposed Solution")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Generating parser and AST with BNFC

  Input LBNF grammar for expressions:

  ```
  EVar.   Exp3 ::= Ident ;
  ETrue.  Exp3 ::= "true" ;
  EFalse. Exp3 ::= "false" ;
  ENat.   Exp3 ::= Integer ;
  EApp.   Exp2 ::= Exp2 Exp3 ;
  EAbs.   Exp1 ::= "λ" Pattern "." ScopedExp ;
  ELet.   Exp1 ::= "let" Pattern "=" Exp1 "in" ScopedExp ;

  PatternVar. Pattern ::= Ident ;
  ScopedExp. ScopedExp ::= Exp1 ;
  ```
], footer: "Implementation")
#slide([
  = Generating parser and AST with BNFC

  Input LBNF grammar for types:

  ```
  token UVarIdent ('?' letter (letter | digit | '_')*) ;

  TUVar.   Type2 ::= UVarIdent ;
  TNat.    Type2 ::= "Nat" ;
  TBool.   Type2 ::= "Bool" ;
  TVar.    Type2 ::= Ident ;
  TArrow.  Type1 ::= Type2 "->" Type1 ;
  TForAll. Type ::= "forall" TypePattern "." ScopedType ;

  TPatternVar. TypePattern ::= Ident ;
  ScopedType. ScopedType ::= Type1 ;
  ```
], footer: "Implementation")
#slide([
  = Generating parser and AST with BNFC

  ```hs
  newtype UVarIdent = UVarIdent String

  data TypePattern = TPatternVar Ident

  data Type
    = TUVar UVarIdent
    | TNat
    | TBool
    | TVar Ident
    | TArrow Type Type
    | TForAll TypePattern ScopedType

  data ScopedType = ScopedType Type
  ```
], footer: "Implementation")
#slide([
  = Generating SOAS with free-foil

  ```hs
  mkSignature ''Raw.Type ''Raw.Ident ''Raw.ScopedType ''Raw.TypePattern
  deriveBifunctor ''TypeSig
  ...
  ```

  ```hs
  data TypeSig scope term
    = TUVarSig UVarIdent
    | TNatSig
    | TBoolSig
    | TArrowSig term term
    | TForAllSig scope

  instance Bifunctor TypeSig where ...
  ```
], footer: "Implementation")
#slide([
  = Generating SOAS with free-foil

  ```hs
  type Type n = AST FoilTypePattern TypeSig n
  type Type' = Type Foil.VoidS

  type Exp n = AST FoilPattern ExpSig n
  type Exp' = Exp Foil.VoidS
  ```
], footer: "Implementation")
#slide([
  = Scope-safe and generic SOAS: benefits

  ```hs
  applySubst
    :: (Foil.Distinct n)
    => (Raw.UVarIdent, Type n) -> Type n -> Type n

  applySubst (ident, type_) (TUVar x)
    | ident == x  = type_
    | otherwise   = TUVar x

  applySubst subst (FreeFoil.Node node) =
    FreeFoil.Node (bimap (applySubstScoped subst) (applySubst subst) node)
  ```

  - Haskell's type system ensures that SOAS invariants are preserved;
  - Extending language with new types will not require any modifications.
], footer: "Implementation")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Typing context

  ```hs
  data TypingContext n = TypingContext
    {
    , tcConstraints :: [(Type', Type')]
    , tcSubst       :: Map UVarIdent (Type')
    , tcEnv         :: Foil.NameMap n (Type')
    , tcLevelMap    :: HashMap UVarIdent Int
    , tcLevel       :: Int
    , tcFreshId     :: Int
    }
  ```
], footer: "Implementation")
#slide([
  = `TypeInferencer` monad

  ```hs
  newtype TypeInferencer n a = TypeInferencer
    { runTI :: TypingContext n -> Either String (a, TypingContext n) }

  instance Monad (TypeInferencer n) where
    TypeInferencer g >>= f = TypeInferencer $ \ctx -> do
      (x, ctx') <- g ctx
      runTI (f x) ctx'
  ```
], footer: "Implementation")
#show math.equation: set text(size: 36pt)
#slide([
  = Algorithm L implementation

  Auxiliary functions:

  #grid(
    columns: (auto, auto),
    inset: 0pt,
    column-gutter: 32pt,
    align(center+horizon)[
      ```hs
      generalize :: Type' -> TypeInferencer n Type'
      specialize_ :: Type' -> TypeInferencer n Type'
      unify :: TypeInferencer n ()
      ```
    ],
    [
      #align(center+horizon)[#rect()[
        $
          serif("gen") &:: sans("Type") times M times bb("N") arrow sans("Type") \
          serif("spec") &:: sans("Type") arrow sans("Type") times A^* \
          serif("unify") &:: C times cal("S") times M arrow cal("S") times M
        $
      ]]
    ],
  )

  Inference function:
  #grid(
    columns: (auto, auto),
    inset: 0pt,
    column-gutter: 32pt,
    align(center+horizon)[
      ```hs
      inferType :: Exp n -> TypeInferencer n Type'
      ```
    ],
    [
      #align(center+horizon)[#rect()[
        $
          cal("L") &:: T times sans("Expr") arrow T times sans("Type")
        $
      ]]
    ],
  )

], footer: "Implementation")

#show math.equation: set text(size: 46pt)
#rawFull.update(true)
#set table.cell(breakable: false)
#slide([
  #block(inset: (top: -64pt, left: -42pt, right: -42pt))[
    #table(
      columns: (auto, auto),
      inset: 18pt,
      align: (horizon + center, top + left),

      table.header(
        table.cell(align: horizon + left)[*Typing Rule*],
        table.cell(align: horizon + left)[*Implementation*]
      ),

      hm_abs,
      ```hs
      inferType (EAbs x e) = do
        t <- freshUVar
        t' <- enterScope x t $
          inferType e
        return (TArrow t t')
      ```,

      hm_app,
      ```hs
      inferType (EApp e0 e1) = do
        t0 <- inferType e0
        t1 <- inferType e1
        t' <- freshUVar
        addConstraint (t0, TArrow t1 t')
        return t'
      ```,

      hm_let,
      ```hs
      inferType (ELet e0 x e1) = do
        t <- enterLevel $
          inferType eBound
        unify
        subst <- gets tcSubst
        tGen <- generalize $
          applySubst subst t
        enterScope x tGen $
          inferType e1
      ```,

      hm_var,
      ```hs
      inferType (Var x) = do
        TypingEnv g <- gets tcEnv
        let s = Foil.lookupName x g
        t <- specialize s
        return t
      ```,
    )
  ]
], footer: "Implementation")
#rawFull.update(false)
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Results

  + Introduced the Hindley-Milner-like algorithm $cal(L)$:
    - Level-based generalization;
    - Separated constraint generation and resolution.
  + Implemented the algorithm in Haskell:
    - Defined language grammar and generated parser with BNFC;
    - Generated generic abstract syntax with binders using Free Foil;
  + Covered core functionality with tests and implemented $forall$-equivalence check.

  Nikolai Kudasov, Diana Tomilovskaia, Anastasia Smirnova, Ekaterina Maximova, \ and I have presented a work-in-progress work on a WITS'25#footnote("Workshop on the Implementation of Type Systems").
], footer: "Results & Future Work")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Future Work

  - Outsorce unification — use general and tested implementation
  - Improve genralization implementation
    - Usage of hashmap can be avoided
  - Benchmark implementation
  - *Generalize implementation* — extract to a library
], footer: "Results & Future Work")
////////////////////////////////////////////////////////////////////////////////
#slide([
  #align(center+horizon)[#text(size: 96pt)[Q&A]]
])
////////////////////////////////////////////////////////////////////////////////
#slide(
  [#bibliography("../ref.bib", full: false, title: "References")],
  footer: "References"
)
