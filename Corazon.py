# Instalacion libreria:
# pip install PythonTurtle

# DESARROLLO:
from turtle import *
# Letra S:
color("blue")
begin_fill()
pensize(3)
right(180)
forward(50)
circle(50,180)
left(180)
circle(50,-180)
forward(-50)
left(-90)

# Espaciado
color("white")
forward(10)
right(-90)
forward(120)
right(-90)
forward(10)

# Letra H:
color("green")
forward(200)
left(180)
forward(100)
left(90)
forward(100)
left(90)
forward(100)
left(180)
forward(200)

# Espaciado
color("white")
left(90)
forward(105)

# Corazon
color("red")
begin_fill()
pensize(3)
left(50)
forward(133)
circle(50,200)
right(140)
circle(50, 200)
forward(133)
end_fill()