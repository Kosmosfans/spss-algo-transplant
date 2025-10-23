!> @brief calculate distance utilities
!> @author LC
!> @date 2025-10-22
module distance_utils
  implicit none
  private

  public :: euclidean_distance_sq

contains

  !> @brief calculate squared Euclidean distance between two vectors
  !> @param[in] x first vector
  !> @param[in] y second vector
  !> @return squared Euclidean distance
  pure function euclidean_distance_sq(x, y) result(d2)
    real, intent(in) :: x(:), y(:)
    real :: d2
    d2 = sum((x - y)**2)
  end function euclidean_distance_sq

end module distance_utils