mapear:: (a->b)->[a]->[b]
mapear a [] = []
mapear a (x:xs) = a x : mapear a xs