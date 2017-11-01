bigNum = (^) 5
wahoo = bigNum $ 10

x = print
y = print "woohoo!"
z = x "hello world"


-- a = (+)
b = 5
-- c = a b 10
-- d = a c 200

m = 12 + n 2
n p = 10000 * p

i :: a -> a
i a = a

-- c :: a -> b -> a
c a b = a

-- c'' :: b -> a -> b
c'' x y = x
c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
-- r = id
r = reverse

co :: (b -> c) -> (a -> b) -> a -> c
-- co = (.)
co f g n = f (g n)

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f a = f a

