(*
    We represent a point in two dimensions using a pair, as defined by type point2d in the given prelude. The first component is abscissa (x) and the second component is the ordinate (y). Abscissas grow from left to right and ordinates grow from bottom to top as illustrated by the following schema:

                          ^ (y)
                          |
                          |
                          |
                          |
   -------------------- (0,0) --------------------> (x)
                          |
                          |
                          |
                          |
A tetragon is a polygon with four sides. We represent such an object using a 4-uple of points, as defined by type tetragon in the given prelude. that appear in the following order: the left upper point (lup), the right upper point (rup), the left lower point (llp) and the right lower point (rlp).

Write a function pairwise_distinct of type tetragon -> bool that checks that the points of an input tetragon are pairwise distinct. In the sequel, we assume that all the points are pairwise distinct.
A tetragon is well-formed if the following properties are verified:
The left upper and the left lower points have the lowest abscissa.
Between the two leftmost points, the left upper point has the greatest ordinate.
Between the two rightmost points, the right upper point has the greatest ordinate.
Write a function wellformed of type tetragon -> bool that returns true if and only if the input tetragon is well formed.
A simple way to rotate a tetragon by 90 degrees clockwise with respect to (0, 0) is to rotate each of its points by exchanging their abscissa and ordinate and negating the resulting ordinate.
Write a function rotate_point of type point2d -> point2d such that rotate_point p is the point p rotated as explained in the previous paragraph.
Once rotated, the points of the tetragon may not be in the right order: lup may be now llp, rup may be now llp, etc.
Write a function reorder of type point2d * point2d * point2d * point2d -> tetragon that takes 4 pairwise distinct points (not necessarily the output of the previous function but any 4 points) and returns a wellformed tetragon.
Write a function rotate_tetragon that takes a well-formed tetragon and returns a well-formed rotated tetragon.
*)
type point2d = int * int
type tetragon = point2d * point2d * point2d * point2d

let pairwise_distinct (lup, rup, llp, rlp) =
  lup <> rup && lup <> llp && lup <> rlp && rup <> llp && rup <> rlp && llp <> rlp;;

let wellformed (lup, rup, llp, rlp) =
  let (x_lup, y_lup) = lup in
  let (x_rup, y_rup) = rup in
  let (x_llp, y_llp) = llp in
  let (x_rlp, y_rlp) = rlp in
  x_lup = min x_lup x_rup && x_lup = min x_lup x_rlp &&
  x_llp = min x_llp x_rup && x_llp = min x_llp x_rlp &&
  y_llp = min y_llp y_lup && y_rlp = min y_rlp y_rup;;

let rotate_point (x, y) =
  (y, -x);;

let reorder (p1, p2, p3, p4) =
  let points = List.sort (fun (x1, y1) (x2, y2) ->
      if x1 = x2 then compare y2 y1 else compare x1 x2) [p1; p2; p3; p4] in 
  match points with
  | [a; b; c; d] ->
      
      let left_upper, left_lower = if fst a < fst b then a, b else b, a in
      let right_upper, right_lower = if fst c < fst d then c, d else d, c in

      let lup, llp = if snd left_upper > snd left_lower then left_upper, left_lower else left_lower, left_upper in
      let rup, rlp = if snd right_upper > snd right_lower then right_upper, right_lower else right_lower, right_upper in
      (lup, rup, llp, rlp)
                                      
let rotate_tetragon (lup, rup, llp, rlp) =
  let rot_lup = rotate_point lup in
  let rot_rup = rotate_point rup in
  let rot_llp = rotate_point llp in
  let rot_rlp = rotate_point rlp in
  reorder (rot_lup, rot_rup, rot_llp, rot_rlp);;
