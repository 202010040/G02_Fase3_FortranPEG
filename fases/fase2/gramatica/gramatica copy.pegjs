{
	type :: node	
		integer :: value
		type(node), pointer :: next => null()
	end type node

	type(node), pointer :: head => null()

  	contains

	subroutine push(value)
		integer, intent(in) :: value
	  	type(node), pointer :: tmp
      		if (associated(head)) then
        		allocate(tmp)
	      		tmp%value = value
	      		tmp%next => head
	      		head => tmp
      		else
        		allocate(head)
        		head%value = value
        		head%next => null()
    		end if	
  	end subroutine push
	
	subroutine show()
	  	type(node), pointer :: tmp
	  	tmp => head
	  	do while (associated(tmp))
		  	print *, tmp%value
		  	tmp => tmp%next
	  	end do
  	end subroutine show

}

s = n:e* {
	type(node), pointer :: res	
	integer i
	do i = 1, size(n)
		call push(n(i))
	end do
	res => head
}

e = n:num sep { 
	integer :: res
	res = n
}

num = n:[0-9]+ {
  	integer :: res
  	read(n, *) res
}

sep = "\n"