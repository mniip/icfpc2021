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
    helper poly acc = helper (delete ear poly) ((prev, ear, next):acc)
      where
        Polygon vertices = mkPolygon poly
        Just (prev, ear, next) = find isEar $ map vertexToTriple vertices
        vertexToTriple :: PolyVertex -> (V2, V2, V2)
        vertexToTriple (PolyVertex v prev next) = (pVertex prev, v, pVertex next)

        isEar :: (V2, V2, V2) -> Bool
        isEar (prev, cur, next) = onTheRight (prev, next) cur && not (any (triangleContainsPt (prev, cur, next)) (filter (\v -> v /= prev && v /= cur && v /= next) poly))


crossProduct :: V2 -> V2 -> Int
crossProduct (V2 ux uy) (V2 vx vy) =  ux * vy - uy * vx

-- https://neerc.ifmo.ru/wiki/index.php?title=Предикат_"левый_поворот"
onTheRight :: (V2, V2) -> V2 -> Bool
onTheRight (p1, p2) p = crossProduct (p .-. p1) (p2 .-. p1) > 0

-- https://mathworld.wolfram.com/TriangleInterior.html
triangleContainsPt :: (V2, V2, V2) -> V2 -> Bool
triangleContainsPt (v0, v1, v2) v = a >= 0 && b >= 0 && a + b <= 1
  where
    det :: V2 -> V2 -> Float
    det u v = fromIntegral $ crossProduct u v
    a = (det v v2 - det v0 v2) / det v1 v2
    b = -(det v v1 - det v0 v1) / det v1 v2

remove :: Polygon -> PolyVertex -> Polygon
remove (Polygon verts) vert = Polygon $ map relink $ filter (\v -> pVertex v /= pVertex vert) verts
  where
    relink v = if      pVertex v == pVertex (pPrevVertex vert) then v {pNextVertex = pNextVertex vert}
               else if pVertex v == pVertex (pNextVertex vert) then v {pPrevVertex = pPrevVertex vert}
               else v
