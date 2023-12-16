! file: error_module.f90

module error_module
  use utility, only : fp
  implicit none

  contains 
  function error_comp(Nx, dt, u, u_prev) result(error)
    real(fp), intent(in) :: dt
    integer, intent(in) :: Nx
    real(fp), intent(in) :: u(0:Nx+1), u_prev(0:Nx+1)
    real(fp) :: error, sum
    integer :: i

    sum = 0.0
    do i = 1, Nx
      sum = sum + abs(u(i) - u_prev(i)) 
    end do
    error = sum /(Nx*dt)
  end function error_comp
end module error_module