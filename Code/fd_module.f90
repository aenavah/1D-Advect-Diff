!file fd_module.f90 

module fd_module
use utility, only : fp

implicit none

contains

function dt_diffusion(C, dx, k) result(timestep)
  implicit none
  real(fp), intent(in) :: C, dx, k
  real(fp) :: timestep  
  timestep = C*((dx**2.0)/(2.0*k))
end function dt_diffusion

function dt_advection(C, a, dx) result(timestep)
  implicit none
  real(fp), intent(in) :: C, a, dx
  real(fp) :: timestep  
  timestep = C*(dx/ABS(a))
end function dt_advection

end module fd_module