!> @brief A module to read numeric data from a CSV file.
!> @author LC
!> @date 2025-10-23
module read_file_module
  implicit none
  contains
    !--------------------------------------------------------------------
    ! Reads numeric data from a CSV file.
    !
    ! Arguments:
    !   file_path - path to the CSV file
    ! Returns:
    !   data      - allocatable 2D array containing the numeric data
    !--------------------------------------------------------------------
    function read_numeric_csv(file_path) result(data)
      character(len=*), intent(in) :: file_path
      real, allocatable :: data(:,:)
      integer :: n_cases, n_vars

      integer :: unit_number, io_status, i
      character(len=1024) :: line

      unit_number = 15
      open(unit=unit_number, file=file_path, status='old', action='read', iostat=io_status)
      if (io_status /= 0) then
        print *, "Error opening file: ", trim(file_path)
        stop
      end if

      ! Count lines (n_cases) and columns (n_vars)
      n_cases = 0
      n_vars = 0
      do
        read(unit_number, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        n_cases = n_cases + 1
        if (n_cases == 1) then
            n_vars = 1
            do i = 1, len_trim(line)
                if (line(i:i) == ',') then
                    n_vars = n_vars + 1
                end if
            end do
        end if
      end do

      rewind(unit_number)

      ! Allocate data array
      allocate(data(n_cases, n_vars))

      ! Read data
      do i = 1, n_cases
        read(unit_number, *, iostat=io_status) data(i, :)
        if (io_status /= 0) then
          print *, "Error reading data at line ", i
          close(unit_number)
          stop
        end if
      end do

      close(unit_number)
    end function read_numeric_csv
end module read_file_module
