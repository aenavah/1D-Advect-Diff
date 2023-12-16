! File: utility.f90
! Purpose: Define useful constants

module utility
  
  implicit none
  

  !! Original Initalization!!
  integer, parameter :: fp = selected_real_kind(15)
  !!End Original Initialization!!

  !!------Deliverables Q6------!!
  !integer, parameter :: fp = selected_real_kind(6)
  !!! End Q6

  integer, parameter :: maxFileLen = 50
  integer, parameter :: maxStrLen = 100
  real (fp), parameter :: pi = acos(-1.0_fp)

contains
  
end module utility
