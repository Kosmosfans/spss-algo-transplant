!> @brief implements core K-Means clustering functionalities
!> @author LC
!> @date 2025-10-22
module kmeans_core
  use distance_utils, only: euclidean_distance_sq
  use initial_centers, only: select_initial_centers
  implicit none
  private

  public :: kmeans

contains

  !--------------------------------------------------------------------
  ! distribute data points to the nearest cluster center.
  !
  ! Arguments:
  !   data        - input dataset (n_cases, n_vars)
  !   centers     - current cluster centers (n_clusters, n_vars)
  !   assignments - output cluster assignments for each data point (n_cases)
  !--------------------------------------------------------------------
  subroutine assign_to_clusters(data, centers, assignments)
    real, intent(in) :: data(:,:), centers(:,:)
    integer, intent(out) :: assignments(:)

    integer :: n_cases, n_clusters
    integer :: i, j
    real :: dist_sq, min_dist_sq
    integer :: best_cluster

    n_cases = size(data, 1)
    n_clusters = size(centers, 1)

    do i = 1, n_cases
      min_dist_sq = huge(1.0)
      best_cluster = -1
      do j = 1, n_clusters
        dist_sq = euclidean_distance_sq(data(i,:), centers(j,:))
        if (dist_sq < min_dist_sq) then
          min_dist_sq = dist_sq
          best_cluster = j
        end if
      end do
      assignments(i) = best_cluster
    end do
  end subroutine assign_to_clusters

  !--------------------------------------------------------------------
  ! adjust cluster centers based on current assignments.
  !
  ! Arguments:
  !   data           - input dataset (n_cases, n_vars)
  !   assignments    - cluster assignments for each data point (n_cases)
  !   n_clusters     - number of clusters
  !   centers        - cluster centers to be updated (n_clusters, n_vars)
  !   cluster_counts - count of data points in each cluster (n_clusters)
  !--------------------------------------------------------------------
  subroutine update_centers(data, assignments, n_clusters, centers, cluster_counts)
    real, intent(in) :: data(:,:)
    integer, intent(in) :: assignments(:), n_clusters
    real, intent(inout) :: centers(:,:)
    integer, intent(out) :: cluster_counts(:)

    integer :: n_vars, n_cases
    integer :: i, j, cluster_idx

    n_vars = size(data, 2)
    n_cases = size(data, 1)

    centers = 0.0
    cluster_counts = 0

    ! cumulate sums for each cluster and count points
    do i = 1, n_cases
      cluster_idx = assignments(i)
      if (cluster_idx > 0) then
        centers(cluster_idx, :) = centers(cluster_idx, :) + data(i, :)
        cluster_counts(cluster_idx) = cluster_counts(cluster_idx) + 1
      end if
    end do

    ! calculate means for each cluster
    do i = 1, n_clusters
      if (cluster_counts(i) > 0) then
        centers(i, :) = centers(i, :) / real(cluster_counts(i))
      end if
    end do
  end subroutine update_centers

  !--------------------------------------------------------------------
  ! perform K-Means clustering
  !
  ! Arguments:
  !   data              - input dataset (n_cases, n_vars)
  !   n_clusters        - number of clusters
  !   max_iter          - maximum number of iterations
  !   final_centers     - output final cluster centers (n_clusters, n_vars)
  !   final_assignments - output final cluster assignments for each data point (n_cases)
  !   final_counts      - output final counts of data points in each cluster (n_clusters)
  !--------------------------------------------------------------------
  subroutine kmeans(data, n_clusters, max_iter, final_centers, final_assignments, final_counts)
    real, intent(in) :: data(:,:)
    integer, intent(in) :: n_clusters, max_iter
    real, intent(out) :: final_centers(:,:)
    integer, intent(out) :: final_assignments(:), final_counts(:)

    integer :: iter
    real, allocatable :: prev_centers(:,:)
    real :: change
    real, parameter :: tolerance = 1e-8

    allocate(prev_centers(size(final_centers,1), size(final_centers,2)))

    ! choose initial centers
    call select_initial_centers(data, n_clusters, final_centers)

    ! iterative refinement
    do iter = 1, max_iter
      prev_centers = final_centers

      ! distribute data points to clusters
      call assign_to_clusters(data, final_centers, final_assignments)

      ! update cluster centers
      call update_centers(data, final_assignments, n_clusters, final_centers, final_counts)

      ! check for convergence
      change = maxval(abs(final_centers - prev_centers))
      if (change < tolerance) then
        print *, 'Convergence reached after ', iter, ' iterations.'
        exit
      end if
    end do

    if (iter > max_iter) then
        print *, 'Maximum iterations reached.'
    end if

    deallocate(prev_centers)

  end subroutine kmeans

end module kmeans_core
