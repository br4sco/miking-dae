let join = foldl concat []

let zip = lam xs. lam ys.
  reverse
    (foldl
       (lam acc. lam y.
         switch acc
         case (_, []) then acc
         case (acc, xs) then (cons (head xs, y) acc, tail xs)
         end)
       ([], xs)
       ys).0
