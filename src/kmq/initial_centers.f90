!> @brief initialize clustering centers for K-means algorithm
!> @author LC
!> @date 2025-10-22
module initial_centers
  use distance_utils, only: euclidean_distance_sq
  implicit none
  private
  public :: select_initial_centers

contains

  subroutine select_initial_centers(data, n_clusters, centers)
    implicit none

    real, intent(in)  :: data(:,:)
    integer, intent(in) :: n_clusters
    real, intent(out) :: centers(:,:)

    integer :: n_cases, n_vars
    integer :: i, j, center_idx
    real, allocatable :: min_dist_sq(:)
    real :: total_dist_sq, r, cumulative_prob

    real :: r_val

    n_cases = size(data, 1)
    n_vars = size(data, 2)

    if (size(centers,1) < n_clusters .or. size(centers,2) /= n_vars) then
      stop "select_initial_centers: centers array has incompatible shape"
    end if
    if (n_cases < n_clusters) then
      stop "select_initial_centers: fewer cases than clusters requested"
    end if

    allocate(min_dist_sq(n_cases))

    ! Select the first center randomly
    call random_seed()
    call random_number(r_val)
    center_idx = 1 + int(r_val * n_cases)
    centers(1, :) = data(center_idx, :)

    ! Select the remaining n_clusters - 1 centers
    do i = 2, n_clusters
      ! For each data point, find the squared distance to the nearest center
      do j = 1, n_cases
        min_dist_sq(j) = huge(1.0)
        do center_idx = 1, i - 1
          min_dist_sq(j) = min(min_dist_sq(j), euclidean_distance_sq(data(j, :), centers(center_idx, :)))
        end do
      end do

      ! Choose the next center with probability proportional to D(x)^2
      total_dist_sq = sum(min_dist_sq)
      call random_number(r_val)
      r = r_val * total_dist_sq
      cumulative_prob = 0.0
      do j = 1, n_cases
        cumulative_prob = cumulative_prob + min_dist_sq(j)
        if (cumulative_prob >= r) then
          centers(i, :) = data(j, :)
          exit
        end if
      end do
    end do

    deallocate(min_dist_sq)

  end subroutine select_initial_centers

end module initial_centers
