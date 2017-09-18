# Homework #1

Don't forget to put contracts, purposes, and your own tests.

1. Define the function ***area-square***, which consumes an integer number denoting the length of two sides and produces the area of the square.
2. Define the function ***volume-cuboid***, which consumes three integer numbers denoting lengths of three sides and produces the volume of the cuboid.
3. Define the function ***is-multiple-of?***, which consumes two integer numbers and returns whether the first number is a multiple of the second one.
4. Define the function ***factorial***, which consumes an integer number and returns the result of the factorial operation.
5. Define the function ***fibonacci***, which consumes an integer number n and returns the n-th fibonacci number. The 3rd fibonacci number is 2.
6. Define the type ***COURSE***, which is either ***CS320***, ***CS311***, or ***CS330***. ***CS320*** has two attributes: ***quiz*** for the number of quizzes and ***homework*** for the number of programming assignments. ***CS311*** has one attribute: ***homework*** which is the number too. ***CS330*** has two attributes: ***projects*** for the number of projects and ***homework***.
7. Define the function ***total-assignments***, which consumes a course and produces the total number of quizzes, homework, and projects for the given course.
8. Define the function ***total-homework***, which consumes a list of courses and produces the total number of homework of the courses in the given list.
9. Define the function ***my-map***, which consumes a function ***f*** and a list of numbers ***l***, and produces a list of numbers generated by applying the input function ***f*** to each element of ***l***. Do not use the map function provided by DrRacket. For example,

		(my-map (lambda (x) (+ 1 x)) (cons 1 (cons 2 (cons 3 empty)))))

	produces

		'(2 3 4)
10. Define the function ***my-filter***, which consumes a predicate ***f*** and a list of numbers ***l***, and produces a list consisting of the elements of ***l*** that satisfy the predicate ***f***. Do not use the filter function provided by DrRacket. For example,

		(my-filter (lambda (x) (> x 1)) (cons 3 (cons 1 (cons 2 empty)))))

	produces

		'(3 2)