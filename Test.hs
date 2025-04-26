triple x = x * 3
triple2 x = x * 3 + 2
circleArea r = pi * (r * r)

area2 d = pi * (r*r)
  where r = d /2
--z = 7 
--x = y ^ 2
--waxOn = x * 5 -- 15^2 * 5 
--y = z + 8 -- 15
waxOn = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2

waxOff x = triple x
