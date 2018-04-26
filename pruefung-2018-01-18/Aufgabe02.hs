{- Aufgabe 2 (5%)
Definieren Sie einen polymorphen algebraischen Datentyp BTplus zur Darstellung von binären Bäumen. Knoten enthalten neben zwei (unabhängigen) polymorphen Elementen zwei Unterbäume. Weiters gibt es auch leere Bäume und leere Unterbäume.
-}
data BTplus a b = EmptyTree | ElementHolder a b (BTplus a b) (BTplus a b) deriving Show -- the deriving clause is for visualization only and not needed for getting points at the exam

{- Aufgabe 3 (5%)
Geben Sie einen konkreten Wert für BTplus sowie seinen genauen Typ an, der die Werte "a" und 2 enthält und genau zwei Knoten aufweist, in Haskell-Syntax mit seinem Typ und als Zeichnung.
-}
a = (ElementHolder "a" 2 (ElementHolder "" 0 EmptyTree EmptyTree) EmptyTree) :: (BTplus String Integer)

{- Aufgabe 4 (10%+10%)
Definieren Sie requal, das für zwei BTplus-Bäume genau dann True ist, wenn beide Bäume Knoten an den gleichen Stellen besitzen und der zweite polymorphe Eintrag jedes Knotens gleich dem ersten Eintrag des anderen an der gleichen Stelle befindlichen Knotens ist. Geben Sie hier eine möglichst allgemeine Deklaration an!
-}

requal :: (Eq a, Eq b) => BTplus a b -> BTplus b a -> Bool
requal EmptyTree EmptyTree			= True
requal EmptyTree (ElementHolder _ _ _ _)	= False
requal (ElementHolder _ _ _ _) EmptyTree	= False
requal (ElementHolder va1 vb1 t11 t12) (ElementHolder vb2 va2 t21 t22)
	| va1 == va2 && vb1 == vb2		= (requal t11 t21) && (requal t12 t22)
	| otherwise				= False

t1 = EmptyTree :: (BTplus Integer Integer)
t2 = EmptyTree :: (BTplus String Integer)
t3 = EmptyTree :: (BTplus Integer String)
t4 = (ElementHolder "a" "b" EmptyTree EmptyTree) :: (BTplus String String)
t5 = (ElementHolder "b" "a" EmptyTree EmptyTree) :: (BTplus String String)
t6 = (ElementHolder "a" 1 (ElementHolder "xy" 2 EmptyTree EmptyTree) EmptyTree) :: (BTplus String Integer)
t7 = (ElementHolder 1 "a" (ElementHolder 2 "xy" EmptyTree EmptyTree) EmptyTree) :: (BTplus Integer String)
t8 = (ElementHolder 1 "a" (ElementHolder 2 "xyz" EmptyTree EmptyTree) EmptyTree) :: (BTplus Integer String)
t9 = (ElementHolder 1 "a" (ElementHolder 3 "xy" EmptyTree EmptyTree) EmptyTree) :: (BTplus Integer String)

{- Aufgabe 5 (5+15%)
 - Definieren Sie filterbt, das eine einstellige boolsche Funktion und zwei BTplus erwartet und eine Liste jener ersten Einträge des ersten Baums und jener zweiten Einträge des zweiten Baums in beliebiger Reihenfolge zurückliefert, für die die boolsche Funktion zutrifft. Geben Sie für filterbt die entsprechende möglichst allgemeine Typdeklaration an.
-}

filterbt :: (a -> Bool) -> BTplus a b -> BTplus c a-> [a]
filterbt _ EmptyTree EmptyTree 						= []
filterbt f EmptyTree (ElementHolder c2 a2 t21 t22)			= [a2 | f a2] ++ (filterbt f EmptyTree t21) ++ (filterbt f EmptyTree t22)
filterbt f (ElementHolder a1 b1 t11 t12) EmptyTree			= [a1 | f a1] ++ (filterbt f t11 EmptyTree) ++ (filterbt f t12 EmptyTree)
filterbt f (ElementHolder a1 b1 t11 t12) (ElementHolder c2 a2 t21 t22)	= [e | e <- [a1,a2], f e] ++ (filterbt f t11 t21) ++ (filterbt f t12 t22)

biggerThanOne :: Integer -> Bool
biggerThanOne n
	| n > 1	= True
	| otherwise = False
