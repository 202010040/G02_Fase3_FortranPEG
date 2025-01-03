program test
    use parser
    implicit none
    
    ! Test cases
    character(len=:), allocatable :: expression
    integer :: result
    
    ! Test 1: Simple addition
    expression = "2 + 3"
    result = parse(expression)
    print *, "Test 1: ", expression, " = ", result
    
    ! Test 2: Simple multiplication
    expression = "4 * 5"
    result = parse(expression)
    print *, "Test 2: ", expression, " = ", result
    
    ! Test 3: Parentheses
    expression = "2 * (3 + 4)"
    result = parse(expression)
    print *, "Test 3: ", expression, " = ", result
    
    ! Test 4: Complex expression
    expression = "2 * 3 + 4 * 5"
    result = parse(expression)
    print *, "Test 4: ", expression, " = ", result
    
    ! Test 5: Nested parentheses
    expression = "(2 + 3) * (4 + 5)"
    result = parse(expression)
    print *, "Test 5: ", expression, " = ", result

end program test