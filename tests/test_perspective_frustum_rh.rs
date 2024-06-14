use approx_cmp::assert_relative_eq;
use cglinalg::{
    Matrix4x4,
    Vector4,
};


#[rustfmt::skip]
#[test]
fn test_perspective_projection_matrix() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let expected = Matrix4x4::new(
        1_f32 / 4_f32, 0_f32,          0_f32,             0_f32,
        0_f32,         2_f32 / 5_f32,  0_f32,             0_f32,
        0_f32,         1_f32 / 5_f32, -100_f32 / 99_f32, -1_f32,
        0_f32,         0_f32,         -100_f32 / 99_f32,  0_f32
    );
    let result = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);

    assert_eq!(result, expected);
}

#[test]
fn test_perspective_projection_maps_left_to_negative_one_in_ndc() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let vector = Vector4::new(left, 0_f32, -2_f32, 1_f32);
    let projected_vector = matrix * vector;
    let expected = -1_f32;
    let result = projected_vector.x;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}

#[test]
fn test_perspective_projection_maps_right_to_positive_one_in_ndc() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let vector = Vector4::new(right, 0_f32, -2_f32, 1_f32);
    let projected_vector = matrix * vector;
    let expected = 1_f32;
    let result = projected_vector.x;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}

#[test]
fn test_perspective_projection_maps_bottom_to_negative_one_in_ndc() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let vector = Vector4::new(0_f32, bottom, -2_f32, 1_f32);
    let projected_vector = matrix * vector;
    let expected = -1_f32;
    let result = projected_vector.y;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}

#[test]
fn test_perspective_projection_maps_top_to_positive_one_in_ndc() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let vector = Vector4::new(0_f32, top, -2_f32, 1_f32);
    let projected_vector = matrix * vector;
    let expected = 1_f32;
    let result = projected_vector.y;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}

#[test]
fn test_perspective_projection_maps_negative_near_to_zero_in_ndc() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let vector = Vector4::new(0_f32, 0_f32, -near, 1_f32);
    let projected_vector = matrix * vector;
    let expected = 0_f32;
    let result = projected_vector.z;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}

#[test]
fn test_perspective_projection_maps_negative_far_to_positive_one_in_ndc() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let vector = Vector4::new(0_f32, 0_f32, -far, 1_f32);
    let projected_vector = matrix * vector;
    let expected = 100_f32;
    let result = projected_vector.z;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}

#[test]
fn test_perspective_projection_preserves_depth_ordering() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let point1 = matrix * Vector4::new(0_f32, 0_f32, -2_f32, 1_f32);
    let point2 = matrix * Vector4::new(0_f32, 0_f32, -99_f32, 1_f32);

    assert!(point1.z < point2.z);
}

#[test]
fn test_perspective_projection_homogeneous_coordinate() {
    let left = -4_f32;
    let right = 4_f32;
    let bottom = -2_f32;
    let top = 3_f32;
    let near = 1_f32;
    let far = 100_f32;
    let matrix = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
    let point = Vector4::new(0_f32, 0_f32, -99_f32, 1_f32);
    let projected_point = matrix * point;
    let expected = 99_f32;
    let result = projected_point.w;

    assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
}
