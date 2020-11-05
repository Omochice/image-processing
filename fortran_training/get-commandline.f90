program get_args
    implicit none
    integer :: i, length, status
    character(:), allocatable :: arg
    intrinsic :: command_argument_count, get_command_argument

    do i = 0, command_argument_count()
        call get_command_argument(i, length=length, status=status)
        if (status == 0) then
            allocate (character(length) :: arg)
            call get_command_argument(i, arg, status=status)

            if (status == 0) then
                if (i == 0) then
                    print *, 'Command = "', arg, '"'
                else
                    print *, 'Argument', i, '= "', arg, '"'
                end if
            end if
            deallocate (arg)
        end if

        if (status /= 0) then
            print *, "Error", status, "on argument", i
        end if
    end do
end program get_args

