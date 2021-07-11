module ICFPC.Triangulate where
import ICFPC.Vector
import ICFPC.Polygon
import Data.List

-- counter-clockwise
triangulate :: [V2] -> [(V2, V2, V2)]
triangulate poly_init = helper poly_init []
  where
    helper :: [V2] -> [(V2, V2, V2)] -> [(V2, V2, V2)]
    helper [v1, v2, v3] acc = (v1, v2, v3):acc
    helper poly acc = case findEarResult of
        Nothing -> acc
        Just _  -> helper (delete ear poly) ((prev, ear, next):acc)
      where
        Polygon vertices = mkPolygon poly
        findEarResult = find isEar $ map vertexToTriple vertices
        Just (prev, ear, next) = findEarResult
        vertexToTriple :: PolyVertex -> (V2, V2, V2)
        vertexToTriple (PolyVertex v prev next) = (pVertex prev, v, pVertex next)

        isEar :: (V2, V2, V2) -> Bool
        isEar (prev, cur, next) = onTheRight (prev, next) cur && not (any (triangleContainsPt (prev, next, cur)) (filter (\v -> v /= prev && v /= cur && v /= next) poly))

crossProduct :: V2 -> V2 -> Int
crossProduct (V2 ux uy) (V2 vx vy) = ux * vy - uy * vx

-- https://neerc.ifmo.ru/wiki/index.php?title=Предикат_"левый_поворот"
onTheRight :: (V2, V2) -> V2 -> Bool
onTheRight (p1, p2) p = crossProduct (p .-. p1) (p2 .-. p1) > 0

triangleContainsPt :: (V2, V2, V2) -> V2 -> Bool
triangleContainsPt (v1, v2, v3) pt = not (has_neg && has_pos)
  where
    sign :: V2 -> V2 -> V2 -> Int
    sign (V2 p1x p1y) (V2 p2x p2y) (V2 p3x p3y) = (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1y - p3y)
    d1 = sign pt v1 v2
    d2 = sign pt v2 v3
    d3 = sign pt v3 v1
    has_neg = d1 < 0 || d2 < 0 || d3 < 0
    has_pos = d1 > 0 || d2 > 0 || d3 > 0

remove :: Polygon -> PolyVertex -> Polygon
remove (Polygon verts) vert = Polygon $ map relink $ filter (\v -> pVertex v /= pVertex vert) verts
  where
    relink v = if      pVertex v == pVertex (pPrevVertex vert) then v {pNextVertex = pNextVertex vert}
               else if pVertex v == pVertex (pNextVertex vert) then v {pPrevVertex = pPrevVertex vert}
               else v
