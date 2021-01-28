let TreeF =
      λ(A : Type) → λ(b : Type) → < LeafF : A | BranchF : { _1 : b, _2 : b } >

let Tree = λ(A : Type) → ∀(b : Type) → (TreeF A b → b) → b

let Leaf
    : ∀(A : Type) → A → Tree A
    = λ(A : Type) →
      λ(a : A) →
      λ(b : Type) →
      λ(make : TreeF A b → b) →
        make ((TreeF A b).LeafF a)

let Branch
    : ∀(A : Type) → Tree A → Tree A → Tree A
    = λ(A : Type) →
      λ(t : Tree A) →
      λ(u : Tree A) →
      λ(b : Type) →
      λ(make : TreeF A b → b) →
        make ((TreeF A b).BranchF { _1 = t b make, _2 = u b make })

let leafList
    : ∀(A : Type) → Tree A → List A
    = λ(A : Type) →
      λ(t : Tree A) →
        t
          (List A)
          ( λ(tf : TreeF A (List A)) →
              merge
                { LeafF = λ(a : A) → [ a ]
                , BranchF = λ(t : { _1 : List A, _2 : List A }) → t._1 # t._2
                }
                tf
          )

let l = Leaf Natural

let b = Branch Natural

in  { exampleTree = b (b (b (l 0) (l 5)) (b (l 4) (l 3))) (l 6)
    , leafList = leafList Natural
    }
