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

#set page(
  width: 1920pt,
  height: 1080pt,
)

#set text(size: 48pt)

#show heading: it => [
  #set text(size: 64pt, weight: 700)
  #block(below: 1.2em, it)
]

// Used to toggle code blocks width between auto/100%
#let rawFull = state("raw-full", false)
#let rawFz = state("raw-font-size", 32pt)

#let setRawFz(size: 32pt) = {
  rawFz.update(size)
}

#show raw: set text(font: ("Geist Mono", "DejaVu Sans Mono"))
#show raw.where(block: true): it => context [
  #set text(size: rawFz.get())
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

#show math.equation: set text(size: 42pt)

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
  + How to make implementation extendable?
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#setRawFz(size: 46pt)
#slide([
  = Hindley-Milner Type System

  - Inference of the most general (principal) type without annotations;

  #align(center+horizon)[
    ```
    let twice = λf.λx. f (f x) in
      let incr = λn. n + 1 in
        (twice incr) 1
    ```
  ]
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Hindley-Milner Type System

  - Inference of the most general (principal) type without annotations;
  - Support of parametric polymorphism;

  #align(center+horizon)[
    ```
    let id = λx.x
      in ... (id 3) ... (id "text") ...
    ```
  ]
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Hindley-Milner Type System

  - Inference of the most general (principal) type without annotations;
  - Support of parametric polymorphism;
  - Soundness and completeness have been proved @Damas1984_TypeAssignment;

  // #columns(2)[
  //   #rect(inset: 16pt, width: 100%)[$
  //     frac(
  //       Gamma tack e_0 : tau #h(40pt) Gamma "," x : accent(Gamma, macron) (tau) tack e_1 : tau',
  //       Gamma tack mono("let") x = e_0 mono("in") e_1 : tau'
  //     )
  //     #h(16pt)
  //     ["Let"]
  //   $]

  //   #colbreak()

  //   #rect(inset: 16pt)[$
  //     frac(
  //       x : sigma in Gamma #h(40pt) tau = italic("inst")(sigma),
  //       Gamma tack x : tau
  //     )
  //     #h(16pt)
  //     ["Var"]
  //   $]
  // ]

  // // quantifies all monotype variables not bound in Γ
  // $
  //   accent(Gamma, macron) (tau) = forall accent(a, hat) . tau ", where"
  //   accent(a, hat) = "free"(tau) - "free"(Gamma)\
  //   "*Calculating" accent(a, hat) "can be slow"
  // $
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Hindley-Milner Type System

  - Inference of the most general (principal) type without annotations;
  - Support of parametric polymorphism;
  - Soundness and completeness have been proved @Damas1984_TypeAssignment;
  - Foundation of type systems in languages such as Haskell, ML, and OCaml;
], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#setRawFz()
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Rémy ranks @Remy1992_SortedEqTheoryTypes in OCaml

  + Each free type variable is assigned with a rank (nesting level)
  + Level is tracked during AST traversal
  + Level can be changed after unification
  + 

  #align(center)[
    ```
    let twice = λf.λx. f (f x) in   -- level 1
      let incr = λn. n + 1 in        -- level 2
        (twice incr) 1                -- level 3
    ```
  ]

], footer: "Context & Related Works")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Rapier @Simon2002_SecretsGHC, Foil @Foil, and Free Foil @FreeFoil

  1. Rapier (2002) — fast, parallelizable, and cacheable technique from GHC
  2. Foil (2022) — type-safe Rapier
  3. Free Foil (2024) — framework that generates scope-safe generic abstract syntax
    + Scope-safe: Foil
    + Generic: data types à la carte @Swierstra2008_a_la_carte

  Thanks Free Foil, program representation is:

  1. Abstract — _easy_ to manipulate with terms;
  2. Generic — _easy_ to add new functionality;
  3. Scope-aware — _easy and safe_ to work with binders.
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
  = Generating Parser & AST with BNFC

  ```
  EVar.    Exp4 ::= Ident ;
  ENat.    Exp4 ::= Integer ;
  EAbs.    Exp1 ::= "λ" Pattern "." ScopedExp ;
  EApp.    Exp2 ::= Exp2 Exp3 ;
  ELet.    Exp1 ::= "let" Pattern "=" Exp1 "in" ScopedExp ;
  ...
  ```

  ```hs
  data Exp
    = EVar Ident
    | ENat Integer
    | EAbs Pattern ScopedExp
    | EApp Exp Exp
    | ELet Pattern Exp ScopedExp
    -- ...
  ```
], footer: "Proposed Solution")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Type Inference Implementation

  ```hs
  newtype TypeInferencer n a = TypeInferencer
    {runTI :: TypingContext n -> Either String (a, TypingContext n)}

  data TypingContext n = TypingContext
    {
    , tcConstraints :: [(Type', Type')]
    , tcSubst       :: Map UVarIdent (Type')
    , tcEnv         :: Foil.NameMap n (Type')
    , tcLevelMap    :: HashMap UVarIdent Int
    , tcLevel       :: Int
    , tcFreshId     :: Int
    }

  inferType :: Exp n -> TypeInferencer n Type'
  ```
], footer: "Proposed Solution")
////////////////////////////////////////////////////////////////////////////////
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

      // Abs
      $
        frac(
          tau = italic("newvar") #h(40pt) Gamma "," x : tau tack e : tau',
          Gamma tack lambda x. e : tau -> tau'
        )
        #h(16pt)
        ["Abs"]
      $,
      ```hs
      inferType (EAbs (FoilPatternVar x) e) = do
        t <- freshUVar
        t' <- enterScope x t (inferType e)
        return (TArrow t t')
      ```,

      // App
      $
        frac(
          Gamma tack e_0 : tau_0 #h(40pt) Gamma tack e_1 : tau_1\
          tau' = italic("newvar") #h(40pt) italic("unify")(tau_0"," tau_1 -> tau'),
          Gamma tack e_0 " " e_1 : tau'
        )
        #h(16pt)
        ["App"]
      $,
      ```hs
      inferType (EApp e0 e1) = do
        t0 <- inferType e0
        t1 <- inferType e1
        t' <- freshUVar
        addConstraints [(t0, TArrow t1 t')]
        return t'
      ```,
      $
        frac(
          Gamma tack e_0 : tau #h(40pt) Gamma "," x : accent(Gamma, macron) (tau) tack e_1 : tau',
          Gamma tack mono("let") x = e_0 mono("in") e_1 : tau'
        )
        #h(16pt)
        ["Let"]
      $,
      ```hs
      inferType (ELet e0 (FoilPatternVar x) e1) = do
        incrLevel
        t <- inferType e0
        decrLevel
        unify
        subst <- gets tcSubst
        tGen <- generalize (applySubst subst t)
        enterScope x tGen (inferType e1)
      ```,
      $
        frac(
          x : sigma in Gamma #h(40pt) tau = italic("inst")(sigma),
          Gamma tack x : tau
        )
        #h(16pt)
        ["Var"]
      $,
      ```hs
      inferType (FreeFoil.Var x) = do
        TypingEnv g <- gets tcEnv
        let s = Foil.lookupName x g
        t <- specialize s
        return t
      ```,
    )
  ]
], footer: "Proposed Solution")
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
  + Covered core functionality with tests and implemented $forall$-equivalence.
  + Presented intermediate results at WITS'25
    - Co-authored with Nikolai Kudasov, Diana Tomilovskaia, Anastasia Smirnova, Ekaterina Maximova
], footer: "Results & Future Work")
////////////////////////////////////////////////////////////////////////////////
#slide([
  = Future Work

  - "Outsorce" unification
  - Improve genralization implementation
    - Usage of hashmap can be avoided
  - Benchmark implementation
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
