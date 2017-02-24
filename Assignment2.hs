
--Nicole Fitzgerald, 10107233, 13nhf
--CISC 260, Assignment 2

module Assignment2 where 
import Data.List

--PROBLEM 1: Write a function called sublistSums that takes a list of Ints 
--and a single Int as parameters.  It must return a list of all of the 
--sub-lists of the first parameter whose sum is equal to the second parameter.  
--If there are no such sub-lists, the function must return [].

--a helper function to divide list into all possible sublists
makeSubs :: [a] -> [[a]]
makeSubs [] = [[]]
makeSubs (x:xs) = makeSubs xs ++ map (x:) (makeSubs xs)

--main function
sublistSums :: [Int] -> Int -> [[Int]]
sublistSums list target =
    filter (\x -> sum x == target) $ makeSubs list



--PROBLEM 2: Write a function called matching that takes two lists as parameters and returns a list as a result.  
--The two parameter lists must be of the same type and the result list must be a list of Ints.  
--The two parameter lists do not have to have the same length.

--REMARK:
--let's zip the two lists together, and from there we can compare elements a and b
--from each zipped tuple (a, b) to check if a == b. If yes, we return the index
--of that tuple in the list

--main function
matching list1 list2 = check (zip ([0..]) (zip list1 list2))

--helper function returning the index of any tuple with matching elements
check list = [x | (x, (y, z)) <- list, y==z]
 




--PROBLEM 3

type Project = (String, String, Int)
type CourseData = [Project]


--main function
legalCourse courseData
  | length (stupidHelper courseData) /= (2 * length courseData) = False
  | otherwise = True

--helper function for legalCourse
stupidHelper courseData = removeDups([ (x, y) | (x, y, z) <- courseData] ++ [ (b, a) | (a, b, c) <- courseData])

--helper function for legalCourse
checkDoubs courseData = [ (x, y) | (x, y, z) <- courseData, x == y] 

--main function
studentCount name theData = length (allProjects name theData)

--helper function for studentCount, creating a list of all occurences of a given name in the course data
allProjects name theData = [ x | (x, y, z) <- theData, x == name] ++ [ y | (x, y, z) <- theData, y == name]

--main function
courseAvg theData = realToFrac(sum (allCourseMarks theData)) / genericLength (allCourseMarks theData)

--helper function for courseAvg, creating a list of all course marks
allCourseMarks theData = [ z | (x, y, z) <- theData]

--main function
studentAvg name theData = realToFrac(sum (allStudentMarks name theData)) / genericLength (allStudentMarks name theData)

--helper function for studentAvg creating a list of all marks recieved by a given student
allStudentMarks name theData = [ z | (x, y, z) <- theData, x == name || y == name]

--main function
partners name theData = [ x | (x, y, z) <- theData, y == name] ++ [y | (x, y, z) <- theData, x == name]

--main function
students theData = removeDups ([x | (x, y, z) <- theData] ++ [y | (x, y, z) <- theData])

--helper function, aptly named
removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . group . sort















