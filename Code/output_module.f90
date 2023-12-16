!file: output_module.f90

module output_module
  use utility, only : maxFileLen, maxStrLen, fp
  !use setup_module, only : filename

implicit none 

contains 
  subroutine write_data(filename, u, x, x_a, x_b)
    use utility, only : maxStrLen, fp 

    implicit none 
    real(fp) :: u(:), x(:), x_a, x_b
    character(len=maxStrLen) :: filename

    ! write data to file 
    open(unit = 20, file = filename)

    write(20, *) u 
    write(20, *) x
    close(20)
  end subroutine write_data


  subroutine write_data_advection_x(filename, x)
    use utility, only : maxStrLen, fp 
    implicit none 
    real(fp) :: x(:)
    character(len=maxStrLen) :: filename
        ! write data to file 
    open(unit = 20, file = filename)

    write(20, *) x
    close(20)
  end subroutine 

  subroutine write_data_advection_sol(filename, u) !row appending 
    use utility, only : maxStrLen, fp 
    implicit none 
    real(fp) :: u(:)
    character(len=maxStrLen) :: filename
    open(unit=20, file=filename, status='old', action='write', position='append')
    write(20, *) u 
    close(20)

  end subroutine  
end module output_module