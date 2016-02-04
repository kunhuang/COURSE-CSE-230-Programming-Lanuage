-- ---
-- title: Homework #1, Due Monday 1/26/15
-- ---


-- Haskell Formalities
-- -------------------

-- We declare that this is the Hw1 module and import some libraries:

module Hw1 where
import SOE
import Play
import XMLTypes

-- Part 0: All About You
-- ---------------------

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Kun Huang"
myEmail = "kuh004@ucsd.edu"
mySID   = "A53097903"

-- Part 1: Defining and Manipulating Shapes
-- ----------------------------------------

-- You will write all of your code in the `Hw1.hs` file, in the spaces
-- indicated. Do not alter the type annotations --- your code must
-- typecheck with these types to be accepted.

-- The following are the definitions of shapes:

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show
-- >
type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

-- 1. Below, define functions `rectangle` and `rtTriangle` as suggested
--    at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
--    built with the Polygon constructor.

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = Polygon [(0, 0), (0, s2), (s1, 0), (s1, s2)]

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [(0, 0), (0, s2), (s1, 0)]

-- 2. Define a function

sides :: Shape -> Int
sides (Rectangle _ _)  = 4
sides (Ellipse _ _)    = 42
sides (RtTriangle _ _) = 3
sides (Polygon vtx)    = if length vtx <= 2 then 0 else length vtx


--   which returns the number of sides a given shape has.
--   For the purposes of this exercise, an ellipse has 42 sides,
--   and empty polygons, single points, and lines have zero sides.

-- 3. Define a function

bigger :: Shape -> Float -> Shape
bigger (Rectangle s1 s2) e  = Rectangle (s1*sqrt e) (s2*sqrt e)
bigger (Ellipse r1 r2) e    = Ellipse (r1*sqrt e) (r2*sqrt e)
bigger (RtTriangle s1 s2) e = Rectangle (s1*sqrt e) (s2*sqrt e)
bigger (Polygon vtx) e      = Polygon (map (expande (sqrt e)) vtx)
  where expande e (x, y)    = (x*e, y*e)



--   that takes a shape `s` and expansion factor `e` and returns
--   a shape which is the same as (i.e., similar to in the geometric sense)
--   `s` but whose area is `e` times the area of `s`.

-- 4. The Towers of Hanoi is a puzzle where you are given three pegs,
--    on one of which are stacked $n$ discs in increasing order of size.
--    To solve the puzzle, you must move all the discs from the starting peg
--    to another by moving only one disc at a time and never stacking
--    a larger disc on top of a smaller one.

--    To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:

--    1. Move $n - 1$ discs from peg $a$ to peg $c$.
--    2. Move the remaining disc from peg $a$ to peg $b$.
--    3. Move $n - 1$ discs from peg $c$ to peg $b$.

--    Write a function

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 1 a b _ = putStrLn ("move disc from " ++ a ++ " to " ++ b)
hanoi n a b c = do
  hanoi (n-1) a c b
  hanoi 1 a b c
  hanoi (n-1) c b a


--   that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
--   where a is the starting peg,
--   emits the series of moves required to solve the puzzle.
--   For example, running `hanoi 2 "a" "b" "c"`

--   should emit the text

-- ~~~
-- move disc from a to c
-- move disc from a to b
-- move disc from c to b
-- ~~~

-- Part 2: Drawing Fractals
-- ------------------------

-- 1. The Sierpinski Carpet is a recursive figure with a structure similar to
--    the Sierpinski Triangle discussed in Chapter 3:

-- ![Sierpinski Carpet](/static/scarpet.png)

-- Write a function `sierpinskiCarpet` that displays this figure on the
-- screen:
minSide = 2

sierpinskiCarpet :: IO ()
sierpinskiCarpet = 
  runGraphics (
    do w <- openWindow "Sierpinski Carpet" (300, 300)
       sierpinski w 290 0 0
       k <- getKey w
       closeWindow w
    )

sierpinski :: Window -> Int -> Int -> Int -> IO()
sierpinski w s x y = 
  if s <= minSide then fillRectangle w s x y
  else do
    sierpinski w newS x y
    sierpinski w newS x (y+newS)
    sierpinski w newS x (y+2*newS)
    sierpinski w newS (x+newS) y
    sierpinski w newS (x+newS) (y+2*newS)
    sierpinski w newS (x+2*newS) y
    sierpinski w newS (x+2*newS) (y+newS)
    sierpinski w newS (x+2*newS) (y+2*newS)
      where newS = div s 3

fillRectangle :: Window -> Int -> Int -> Int -> IO()
fillRectangle w s x y = drawInWindow w (withColor Blue (SOE.polygon [(x,y),(x+s,y),(x+s,y+s),(x,y+s)])) 

-- Note that you either need to run your program in `SOE/src` or add this
-- path to GHC's search path via `-i/path/to/SOE/src/`.
-- Also, the organization of SOE has changed a bit, so that now you use
-- `import SOE` instead of `import SOEGraphics`.

-- 2. Write a function `myFractal` which draws a fractal pattern of your
--    own design.  Be creative!  The only constraint is that it shows some
--    pattern of recursive self-similarity.

myFractal :: IO ()
myFractal =
  runGraphics (
    do w <- openWindow "My Fractal Carpet" (300, 300)
       myFractalUnit w 290 0 0
       k <- getKey w
       closeWindow w
    )

myFractalUnit :: Window -> Int -> Int -> Int -> IO()
myFractalUnit w s x y = 
  if s <= minSide then fillRectangle w s x y
  else do
    myFractalUnit w newS x y
    myFractalUnit w newS x (y+2*newS)
    myFractalUnit w newS (x+newS) (y+newS)
    myFractalUnit w newS (x+2*newS) y
    myFractalUnit w newS (x+2*newS) (y+2*newS)
      where newS = div s 3

-- Part 3: Recursion Etc.
-- ----------------------

-- First, a warmup. Fill in the implementations for the following functions.

-- (Your `maxList` and `minList` functions may assume that the lists
-- they are passed contain at least one element.)

-- Write a *non-recursive* function to compute the length of a list

lengthNonRecursive :: [a] -> Int
lengthNonRecursive = foldl (\n _ -> n+1) 0

-- `doubleEach [1,20,300,4000]` should return `[2,40,600,8000]`

doubleEach :: [Int] -> [Int]
doubleEach (x:xs) = (x*2):(doubleEach xs)

-- Now write a *non-recursive* version of the above.

doubleEachNonRecursive :: [Int] -> [Int]
doubleEachNonRecursive = map (*2)

-- `pairAndOne [1,20,300]` should return `[(1,2), (20,21), (300,301)]`

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne (x:xs) = (x, x+1) : pairAndOne xs


-- Now write a *non-recursive* version of the above.

pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
pairAndOneNonRecursive = map (\x -> (x, x+1))

-- `addEachPair [(1,2), (20,21), (300,301)]` should return `[3,41,601]`

addEachPair :: [(Int, Int)] -> [Int]
addEachPair ((x0, x1):xs) = (x0 + x1) : addEachPair xs

-- Now write a *non-recursive* version of the above.

addEachPairNonRecursive :: [(Int, Int)] -> [Int]
addEachPairNonRecursive = map (\(x0, x1) -> (x0 + x1))

-- `minList` should return the *smallest* value in the list. You may assume the
-- input list is *non-empty*.

minList :: [Int] -> Int
minList (x:[]) = x
minList (x:xs) = min x (minList xs)

-- Now write a *non-recursive* version of the above.

minListNonRecursive :: [Int] -> Int
minListNonRecursive (x:xs) = foldl min x xs

-- `maxList` should return the *largest* value in the list. You may assume the
-- input list is *non-empty*.

maxList :: [Int] -> Int
maxList (x:[]) = x
maxList (x:xs) = max x (maxList xs)


-- Now write a *non-recursive* version of the above.

maxListNonRecursive :: [Int] -> Int
maxListNonRecursive (x:xs) = foldl max x xs

-- Now, a few functions for this `Tree` type.

data Tree a = Leaf a | Branch (Tree a) (Tree a)
              deriving (Show, Eq)

-- `fringe t` should return a list of all the values occurring as a `Leaf`.
-- So: `fringe (Branch (Leaf 1) (Leaf 2))` should return `[1,2]`

fringe :: Tree a -> [a]
fringe (Leaf a) = [a]
fringe (Branch a b) = fringe a ++ fringe b
--Why wrong
--fringe (Branch (Tree a) (Tree b)) = (fringe (Tree a)) ++ (fringe (Tree b))


-- `treeSize` should return the number of leaves in the tree.
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

treeSize :: Tree a -> Int
treeSize (Leaf a) = 1
treeSize (Branch a b) = treeSize a + treeSize b

-- `treeSize` should return the height of the tree.
-- So: `height (Branch (Leaf 1) (Leaf 2))` should return `1`.

treeHeight :: Tree a -> Int
treeHeight (Leaf a) = 0
treeHeight (Branch a b) = if treeSize a > treeSize b then treeSize a + 1 else treeSize b + 1

-- Now, a tree where the values live at the nodes not the leaf.

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
                      deriving (Show, Eq)

-- `takeTree n t` should cut off the tree at depth `n`.
-- So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `IBranch 1 ILeaf ILeaf`.

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree 0 _ = ILeaf
takeTree n ILeaf = ILeaf
takeTree n (IBranch a t0 t1) = IBranch a (takeTree (n-1) t0) (takeTree (n-1) t1)

--Not in scope: data constructor ��InternalTree��
--Why wrong
--takeTree 0 (InternalTree a) = ILeaf
--takeTree n ILeaf = ILeaf
--takeTree n (IBranch a (InternalTree b) (InternalTree c)) = IBranch a (takeTree (n-1) (InternalTree b)) (takeTree (n-1) (InternalTree c))

-- `takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
-- So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile _ ILeaf = ILeaf
takeTreeWhile p (IBranch a t0 t1) 
                      | p a       = IBranch a (takeTreeWhile p t0) (takeTreeWhile p t1) 
                      | otherwise = ILeaf

-- Write the function map in terms of foldr:

myMap :: (a -> b) -> [a] -> [b]
--Worse
--ssmyMap f (x:xs) = foldl (\a b -> a++[f b]) [f x] xs

myMap f = foldr (\x xs -> f x : xs) []


-- Part 4: Transforming XML Documents
-- ----------------------------------

-- The rest of this assignment involves transforming XML documents.
-- To keep things simple, we will not deal with the full generality of XML,
-- or with issues of parsing. Instead, we will represent XML documents as
-- instances of the following simpliﬁed type:

-- ~~~~
-- data SimpleXML =
--    PCDATA String
--  | Element ElementName [SimpleXML]
--  deriving Show

-- type ElementName = String
-- ~~~~

-- That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
-- data") node containing a string or else an `Element` node containing a
-- tag and a list of sub-nodes.

-- The file `Play.hs` contains a sample XML value. To avoid getting into
-- details of parsing actual XML concrete syntax, we'll work with just
-- this one value for purposes of this assignment. The XML value in
-- `Play.hs` has the following structure (in standard XML syntax):

-- ~~~
-- <PLAY>
--   <TITLE>TITLE OF THE PLAY</TITLE>
--   <PERSONAE>
--     <PERSONA> PERSON1 </PERSONA>
--     <PERSONA> PERSON2 </PERSONA>
--     ... -- MORE PERSONAE
--     </PERSONAE>
--   <ACT>
--     <TITLE>TITLE OF FIRST ACT</TITLE>
--     <SCENE>
--       <TITLE>TITLE OF FIRST SCENE</TITLE>
--       <SPEECH>
--         <SPEAKER> PERSON1 </SPEAKER>
--         <LINE>LINE1</LINE>
--         <LINE>LINE2</LINE>
--         ... -- MORE LINES
--       </SPEECH>
--       ... -- MORE SPEECHES
--     </SCENE>
--     ... -- MORE SCENES
--   </ACT>
--   ... -- MORE ACTS
-- </PLAY>
-- ~~~

-- * `sample.html` contains a (very basic) HTML rendition of the same
--   information as `Play.hs`. You may want to have a look at it in your
--   favorite browser.  The HTML in `sample.html` has the following structure
--   (with whitespace added for readability):

-- ~~~
-- <html>
--   <body>
--     <h1>TITLE OF THE PLAY</h1>
--     <h2>Dramatis Personae</h2>
--     PERSON1<br/>
--     PERSON2<br/>
--     ...
--     <h2>TITLE OF THE FIRST ACT</h2>
--     <h3>TITLE OF THE FIRST SCENE</h3>
--     <b>PERSON1</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <b>PERSON2</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <h3>TITLE OF THE SECOND SCENE</h3>
--     <b>PERSON3</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--   </body>
-- </html>
-- ~~~

-- You will write a function `formatPlay` that converts an XML structure
-- representing a play to another XML structure that, when printed,
-- yields the HTML speciﬁed above (but with no whitespace except what's
-- in the textual data in the original XML).
formatContent :: Int -> [SimpleXML] -> [SimpleXML]
formatContent _ []                                = []
formatContent l ((Element tag content):xmls)  
                | tag == "PERSONAE"              = [Element "h2" [PCDATA "Dramatis Personae"]] ++ formatContent l content ++ formatContent l xmls
                | tag == "ACT"                   = formatContent l content ++ formatContent l xmls
                | tag == "SCENE"                 = formatContent l content ++ formatContent l xmls
                | tag == "SPEECH"                = (formatContent l content) ++ formatContent l xmls
                | tag == "SPEAKER"               = [Element "b" content] ++ [PCDATA "<br/>"] ++ formatContent l xmls
                | tag == "TITLE"                 = [Element ("h"++(show l)) content] ++ formatContent (l+1) xmls
                | tag `elem` ["LINE","PERSONA"]  = content ++ [PCDATA "<br/>"] ++ formatContent l xmls
                | otherwise                      = error "No such tag"


formatPlay :: SimpleXML -> SimpleXML
formatPlay (Element tag content)
                | tag == "PLAY"  = Element "html" [Element "body" (formatContent 1 content)]
                | otherwise      = error "Invalid Play"


-- The main action that we've provided below will use your function to
-- generate a ﬁle `dream.html` from the sample play. The contents of this
-- ﬁle after your program runs must be character-for-character identical
-- to `sample.html`.

mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
             testResults "dream.html" "sample.html"
-- >
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds)
     | c==d = firstDiff cs ds
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)
-- >
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> do
      putStr "Success!\n"
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 20 cs)
      putStr "' vs '"
      putStr (take 20 ds)
      putStr "'\n"

-- Important: The purpose of this assignment is not just to "get the job
-- done" --- i.e., to produce the right HTML. A more important goal is to
-- think about what is a good way to do this job, and jobs like it. To
-- this end, your solution should be organized into two parts:

-- 1. a collection of generic functions for transforming XML structures
--    that have nothing to do with plays, plus

-- 2. a short piece of code (a single deﬁnition or a collection of short
--    deﬁnitions) that uses the generic functions to do the particular
--    job of transforming a play into HTML.

-- Obviously, there are many ways to do the ﬁrst part. The main challenge
-- of the assignment is to ﬁnd a clean design that matches the needs of
-- the second part.

-- You will be graded not only on correctness (producing the required
-- output), but also on the elegance of your solution and the clarity and
-- readability of your code and documentation.  Style counts.  It is
-- strongly recommended that you rewrite this part of the assignment a
-- couple of times: get something working, then step back and see if
-- there is anything you can abstract out or generalize, rewrite it, then
-- leave it alone for a few hours or overnight and rewrite it again. Try
-- to use some of the higher-order programming techniques we've been
-- discussing in class.

-- Submission Instructions
-- -----------------------

-- * If working with a partner, you should both submit your assignments
--   individually.

-- * Make sure your `Hw1.hs` is accepted by GHCi without errors or warnings.

-- * Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
--   subject "HW1" (minus the quotes). *This address is unmonitored!*

-- Credits
-- -------

-- This homework is essentially Homeworks 1 & 2 from
-- <a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
