module Colors (allColors) where 

import Graphics.UI.GLUT

red    = Color3 1 0 0
green  = Color3 0 1 0
blue   = Color3 0 0 1
yellow = Color3 1 1 0
violet = Color3 1 0.5 1
orange = Color3 1 0.5 0
grey   = Color3 0.4 0.4 0.4

allColors :: [Color3 GLfloat]
allColors = [ red, green, blue, yellow, violet, orange, grey ]
