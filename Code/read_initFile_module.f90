
module read_initFile_module
use utility, only: fp, maxFileLen, maxStrLen
implicit none 

!read_initFileInt(inFile, varName)
!read_initFileReal(inFile, varName)
!read_initFileChar(inFile, varName)


contains 
function read_initFileInt(inFile,varName) result(varValue)
!read in real 
  implicit none
  character(len=*),intent(IN) :: inFile,varName
  integer :: varValue

  integer :: i,openStatus,inputStatus
  integer :: simInitVars
  character(len=maxStrLen) :: simCharVars
  integer :: pos1,pos2

  open(unit = 11, file=inFile, status='old',IOSTAT=openStatus,FORM='FORMATTED',ACTION='READ')

  do i=1,maxFileLen
    read(11, FMT = 101, IOSTAT=inputStatus) simCharVars
    pos1 = index(simCharVars,varName)
    pos2 = pos1+len_trim(varName)
    if (pos2 > len_trim(varName)) then
      read(simCharVars(pos2+1:),*)simInitVars
      varValue = simInitVars
    endif
  end do

  close(11)

101 FORMAT(A, 1X, I5)

end function read_initFileInt

!----------------------------------------
function read_initFileReal(inFile,varName) result(varValue)
  !read in real 
    implicit none
    character(len=*),intent(IN) :: inFile,varName
    real(fp) :: varValue
  
    integer :: i,openStatus,inputStatus
    real(fp) :: simInitVars
    character(len=maxStrLen) :: simCharVars
    integer :: pos1,pos2
  
    open(unit = 11, file=inFile, status='old',IOSTAT=openStatus,FORM='FORMATTED',ACTION='READ')
  
    do i=1,maxFileLen
      read(11, FMT = 101, IOSTAT=inputStatus) simCharVars
      pos1 = index(simCharVars,varName)
      pos2 = pos1+len_trim(varName)
      if (pos2 > len_trim(varName)) then
        read(simCharVars(pos2+1:),*)simInitVars
        varValue = simInitVars
      endif
    end do
  
    close(11)
  
  101 FORMAT(A, 1X, I5)
  
  end function read_initFileReal

!----------------------------------------
  function read_initFileChar(inFile,varName) result(varValue)
    !read in char
      implicit none
      character(len=*),intent(IN) :: inFile,varName
      character(len=maxStrLen) :: varValue
    
      integer :: i,openStatus,inputStatus
      character :: simInitVars
      character(len=maxStrLen) :: simCharVars
      integer :: pos1,pos2
    
      open(unit = 11, file=inFile, status='old',IOSTAT=openStatus,FORM='FORMATTED',ACTION='READ')
    
      do i=1,maxFileLen
        read(11, FMT = 101, IOSTAT=inputStatus) simCharVars
        pos1 = index(simCharVars,varName)
        pos2 = pos1+len_trim(varName)
        if (pos2 > len_trim(varName)) then
          read(simCharVars(pos2+1:),*)simInitVars
          varValue = simInitVars
        endif
      end do
    
      close(11)
    
    101 FORMAT(A, 1X, I5)
    
    end function read_initFileChar
end module read_initFile_module