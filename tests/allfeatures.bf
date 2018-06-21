write a number 65 in position 1 by multiplying 6 by 10 and adding 5

++++++  this is a 6
[->++++++++++ (10 times)<]  a multiplication loop

> +++++

at this point memory should be (0 65# 0 0 0 etc)

print an A

.

> > + > + > +-+

(0 65 0 1 1 1# 0 0 etc)

[<]

(0 65 0# 1 1 1 0 0 etc)

copy twice the 65 before

<
[->+>+<<]

(0 0# 65 66 1 1 0 0 etc)

Print A and B
>.>.

move to the first 0 to the right
[>]

(0 0 65 66 1 1 0# 0 etc)

put a negative by adding 128 and wrapping
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++

multiply the 66 by 3 putting in the zero

<<<
[->>>+++<<<]


(0 0 65 0# 1 1 70 0 etc)

>>>

print F
.

[<]
<

print A
.

(0 0 65# 0 1 1 70 0 etc)

get some data
<,

add it to 65 in position 0

[-<+>]
>
[-<<+>>]


(65 plus x 0 0# 0 1 1 70 0 etc)

print the result (if a 0 was the input it should print a q (48 plus 65)
<<.
