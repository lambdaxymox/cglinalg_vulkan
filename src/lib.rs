extern crate cglinalg;

use cglinalg::{
    Angle,
    SimdScalarFloat,
    Radians,
    Matrix4x4,
};


/// Construct a new orthographic projection transformation mapping from 
/// an eye space with a right-handed coordinate system to a clip space with a 
/// left-handed coordinate coordinate system compatible with Metal's normalized 
/// device coordinates.
///
/// The source eye space coordinate system is a right-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces up.
/// * The camera space forward direction faces the **negative z-axis**.
/// * The camera space **z-axis** faces the negative forward direction.
/// 
/// The target clip space coordinate system is a left-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces up.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// In particular, we map the eye space **z-axis** to the clip space **z-axis** by
/// changing the sign of the eye space **z coordinate**. The transformation maps 
/// the eye space volume `[left, right] x [bottom, top] x [-far, -near]` to 
/// the normalized device coordinates `[-1, -1] x [-1, 1] x [0, 1]`. The projection
/// matrix is given by
/// ```text
/// [ m[0, 0]  0        0        m[3, 0] ]
/// [ 0        m[1, 1]  0        m[3, 1] ]
/// [ 0        0        m[2, 2]  m[3, 2] ]
/// [ 0        0        0        1       ]
/// ```
/// where
/// ```text
/// m[0, 0] ==  2 / (right - left)
/// m[3, 0] == -(right + left) / (right - left)
/// m[1, 1] ==  2 / (top - bottom)
/// m[3, 1] == -(top + bottom) / (top - bottom)
/// m[2, 2] == -1 / (far - near)
/// m[3, 2] == -near / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// 
/// * `left` is the horizontal position of the left plane in eye space.
/// The left plane is a plane parallel to the **yz-plane** along the **x-axis**.
/// * `right` is the horizontal position of the right plane in eye space.
/// The right plane is a plane parallel to the **yz-plane** along the **x-axis**.
/// * `bottom` is the vertical position of the bottom plane in eye space.
/// The bottom plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `top` is the vertical position of the top plane in eye space.
/// The top plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `near` is the distance along the **negative z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **negative z-axis**.
/// * `far` is the distance along the **negative z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **negative z-axis**.
/// 
/// The parameters must satisfy the following constraints to generate a valid 
/// orthographic projection matrix.
/// ```text
/// left < right
/// bottom < top
/// 0 < near < far
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// #
/// let left = -4_f32;
/// let right = 4_f32;
/// let bottom = -3_f32;
/// let top = 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let expected = Matrix4x4::new(
///     1_f32 / 4_f32, 0_f32,          0_f32,            0_f32,
///     0_f32,         1_f32 / 3_f32,  0_f32,            0_f32,
///     0_f32,         0_f32,         -10_f32 / 999_f32, 0_f32,
///     0_f32,         0_f32,         -1_f32 / 999_f32,  1_f32
/// );
/// let result = cglinalg_metal::orthographic_frustum_rh(left, right, bottom, top, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
/// ```
pub fn orthographic_frustum_rh<S>(left: S, right: S, bottom: S, top: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;

    let c0r0 = two / (right - left);
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = two / (top - bottom);
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 =  zero;
    let c2r1 =  zero;
    let c2r2 = -one / (far - near);
    let c2r3 =  zero;

    let c3r0 = -(right + left) / (right - left);
    let c3r1 = -(top + bottom) / (top - bottom);
    let c3r2 = -near / (far - near);
    let c3r3 =  one;

    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}

/// Construct a new orthographic projection transformation mapping from 
/// an eye space with a left-handed coordinate system to a clip space with a 
/// left-handed coordinate coordinate system compatible with Metal's normalized 
/// device coordinates.
///
/// The source eye space coordinate system is a left-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces up.
/// * The camera space **positive z-axis** faces the forward direction.
/// 
/// The target clip space coordinate system is a left-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces up.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// The transformation maps the eye space volume 
/// `[left, right] x [bottom, top] x [near, far]` to the normalized device coordinates
/// `[-1, -1] x [-1, 1] x [0, 1]`. The projection matrix is given by
/// ```text
/// [ m[0, 0]  0        0        m[3, 0] ]
/// [ 0        m[1, 1]  0        m[3, 1] ]
/// [ 0        0        m[2, 2]  m[3, 2] ]
/// [ 0        0        0        1       ]
/// ```
/// where
/// ```text
/// m[0, 0] ==  2 / (right - left)
/// m[3, 0] == -(right + left) / (right - left)
/// m[1, 1] ==  2 / (top - bottom)
/// m[3, 1] == -(top + bottom) / (top - bottom)
/// m[2, 2] ==  1 / (far - near)
/// m[3, 2] == -near / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order. 
/// 
/// # Parameters
/// 
/// * `left` is the horizontal position of the left plane in eye space.
/// The left plane is a plane parallel to the **yz-plane** along the **x-axis**.
/// * `right` is the horizontal position of the right plane in eye space.
/// The right plane is a plane parallel to the **yz-plane** along the **x-axis**. 
/// * `bottom` is the vertical position of the bottom plane in eye space.
/// The bottom plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `top` is the vertical position of the top plane in eye space.
/// The top plane is a plane parallel to the **zx-plane** along the **positive y-axis**.
/// * `near` is the distance along the **positive z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// * `far` is the distance along the **positive z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
///
/// The parameters must satisfy the following constraints to generate a valid 
/// orthographic projection matrix.
/// ```text
/// left < right
/// bottom < top
/// 0 < near < far
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// #
/// let left = -4_f32;
/// let right = 4_f32;
/// let bottom = -3_f32;
/// let top = 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let expected = Matrix4x4::new(
///     1_f32 / 4_f32, 0_f32,          0_f32,            0_f32,
///     0_f32,         1_f32 / 3_f32,  0_f32,            0_f32,
///     0_f32,         0_f32,          10_f32 / 999_f32, 0_f32,
///     0_f32,         0_f32,         -1_f32 / 999_f32,  1_f32
/// );
/// let result = cglinalg_metal::orthographic_frustum_lh(left, right, bottom, top, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
/// ```
pub fn orthographic_frustum_lh<S>(left: S, right: S, bottom: S, top: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;

    let c0r0 = two / (right - left);
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = two / (top - bottom);
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 = zero;
    let c2r1 = zero;
    let c2r2 = one / (far - near);
    let c2r3 = zero;

    let c3r0 = -(right + left) / (right - left);
    let c3r1 = -(top + bottom) / (top - bottom);
    let c3r2 = -near / (far - near);
    let c3r3 =  one;

    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}

/// Construct a new perspective field of view projection transformation 
/// mapping from an eye space with a right-handed coordinate system to a clip space 
/// with a left-handed coordinate coordinate system compatible with Metal's 
/// normalized device coordinates.
///
/// The source eye space coordinate system is a right-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces up.
/// * The camera space forward direction faces the **negative z-axis**.
/// * The camera space **z-axis** faces the negative forward direction.
/// 
/// The target clip space coordinate system is a left-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces up.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// In particular, we map the eye space **negative z-axis** to the clip space 
/// **positive z-axis** by changing the sign of the eye space **z coordinate**. This 
/// variant of the perspective projection matrix is parametrized using the vertical 
/// field of view angle `vfov` and the aspect ratio `aspect_ratio`. The 
/// aspect ratio `aspect_ratio` is the ratio of the width of the camera viewport to 
/// the height of the camera viewport. Here
/// ```text
/// tan(vfov) == top / near
/// aspect_ratio == right / top
/// ```
/// The transformation maps the eye space frustum volume contained in 
/// `[-right, right] x [-top, top] x [-far, -near]` to the normalized device 
/// coordinates `[-1, -1] x [-1, 1] x [0, 1]`. The projection matrix is given 
/// by
/// ```text
/// [ m[0, 0]  0         0        0       ]
/// [ 0        m[1, 1]   0        0       ]
/// [ 0        0         m[2, 2]  m[3, 2] ]
/// [ 0        0        -1        0       ]
/// ```
/// where
/// ```text
/// m[0, 0] ==  1 / (aspect_ratio * tan(vfov / 2))
/// m[1, 1] ==  1 / tan(vfov / 2)
/// m[2, 2] == -far / (far - near)
/// m[3, 2] == -(far * near) / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// 
/// * `vfov` is the vertical angle in radians of the symmetric viewing frustum
/// centered at the eye position in the **xy-plane**.
/// * `aspect_ratio` is the ratio of the width of the symmmetric viewing frustum to the
/// height of the symmetric viewing frustum.
/// * `near` is the distance along the **negative z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **negative z-axis**.
/// * `far` the distance along the **negative z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **negative z-axis**.
///
/// The parameters must satisfy the following constraints to generate a valid 
/// perspective projection matrix.
/// ```text
/// vfov > 0
/// aspect_ratio > 0
/// 0 < near < far
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// # use cglinalg::Degrees;
/// #
/// # use core::f32;
/// #
/// let vfov = Degrees(72_f32);
/// let aspect_ratio = 4_f32 / 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let tan_vfov_over_two = f32::sqrt(5_f32 - 2_f32 * f32::sqrt(5_f32));
/// let a = 1_f32 / (aspect_ratio * tan_vfov_over_two);
/// let b = 1_f32 / tan_vfov_over_two;
/// let expected = Matrix4x4::new(
///     a,     0_f32,  0_f32,               0_f32,
///     0_f32, b,      0_f32,               0_f32,
///     0_f32, 0_f32, -1000_f32 / 999_f32, -1_f32,
///     0_f32, 0_f32, -100_f32 / 999_f32,   0_f32
/// );
/// let result = cglinalg_metal::perspective_fov_rh(vfov, aspect_ratio, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-6, relative_all <= f32::EPSILON);
/// ```
pub fn perspective_fov_rh<S, A>(vfov: A, aspect_ratio: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
    A: Into<Radians<S>>,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;
    let range = Angle::tan(vfov.into() / two) * near;

    let c0r0 = near / (range * aspect_ratio);
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = near / range;
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 =  zero;
    let c2r1 =  zero;
    let c2r2 = -far / (far - near);
    let c2r3 = -one;

    let c3r0 =  zero;
    let c3r1 =  zero;
    let c3r2 = -(far * near) / (far - near);
    let c3r3 =  zero;

    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}

/// Construct a new perspective field of view projection transformation 
/// mapping from an eye space with a left-handed coordinate system to a clip space 
/// with a left-handed coordinate coordinate system compatible with Metal's 
/// normalized device coordinates.
///
/// The source eye space coordinate system is a left-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces up.
/// * The camera space forward direction faces the **positive z-axis**.
/// 
/// The target clip space coordinate system is a left-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces up.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// In particular, we map the eye space **positive z-axis** to the clip space 
/// **positive z-axis** by changing the sign of the eye space `z` coordinate. This 
/// variant of the perspective projection matrix is parametrized using the vertical 
/// field of view angle `vfov` and the aspect ratio `aspect_ratio`. The 
/// aspect ratio `ascpect_ratio` is the ratio of the width of the camera viewport to 
/// the height of the camera viewport. Here
/// ```text
/// tan(vfov) == top / near
/// aspect_ratio == right / top
/// ```
/// The transformation maps the eye space frustum volume contained in 
/// `[-right, right] x [-top, top] x [near, far]` to the normalized device 
/// coordinates `[-1, -1] x [-1, 1] x [0, 1]`. The projection matrix is given 
/// by
/// ```text
/// [ m[0, 0]  0        0        0       ]
/// [ 0        m[1, 1]  0        0       ]
/// [ 0        0        m[2, 2]  m[3, 2] ]
/// [ 0        0        1        0       ]
/// ```
/// where
/// ```text
/// m[0, 0] ==  1 / (aspect_ratio * tan(vfov / 2))
/// m[1, 1] ==  1 / tan(vfov / 2)
/// m[2, 2] ==  far / (far - near)
/// m[3, 2] == -(far * near) / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order. 
/// 
/// # Parameters
/// 
/// * `vfov` is the vertical angle in radians of the symmetric viewing frustum
/// centered at the eye position in the **xy-plane**.
/// * `aspect_ratio` is the ratio of the width of the symmmetric viewing frustum to the
/// height of the symmetric viewing frustum.
/// * `near` is the distance along the **positive z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// * `far` is the distance along the **positive z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// 
/// The input parameters must satisfy the following constraints to generate a valid 
/// perspective projection matrix.
/// ```text
/// vfov > 0
/// aspect_ratio > 0
/// 0 < near < far
/// ```
///
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// # use cglinalg::Degrees;
/// #
/// # use core::f32;
/// #
/// let vfov = Degrees(72_f32);
/// let aspect_ratio = 4_f32 / 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let tan_vfov_over_two = f32::sqrt(5_f32 - 2_f32 * f32::sqrt(5_f32));
/// let a = 1_f32 / (aspect_ratio * tan_vfov_over_two);
/// let b = 1_f32 / tan_vfov_over_two;
/// let expected = Matrix4x4::new(
///     a,     0_f32,  0_f32,              0_f32,
///     0_f32, b,      0_f32,              0_f32,
///     0_f32, 0_f32,  1000_f32 / 999_f32, 1_f32,
///     0_f32, 0_f32, -100_f32 / 999_f32,  0_f32
/// );
/// let result = cglinalg_metal::perspective_fov_lh(vfov, aspect_ratio, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-6, relative_all <= f32::EPSILON);
/// ```
pub fn perspective_fov_lh<S, A>(vfov: A, aspect_ratio: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
    A: Into<Radians<S>>,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;
    let range = Angle::tan(vfov.into() / two) * near;

    let c0r0 = near / (range * aspect_ratio);
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = near / range;
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 = zero;
    let c2r1 = zero;
    let c2r2 = far / (far - near);
    let c2r3 = one;

    let c3r0 =  zero;
    let c3r1 =  zero;
    let c3r2 = -(far * near) / (far - near);
    let c3r3 =  zero;

    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}


/// Construct a new perspective projection transformation mapping from 
/// an eye space with a right-handed coordinate system to a clip space with a 
/// left-handed coordinate coordinate system compatible with Metal's normalized 
/// device coordinates.
///
/// The source eye space coordinate system is a right-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces up.
/// * The camera space forward direction faces the **negative z-axis**.
/// * The camera space **z-axis** faces the negative forward direction.
/// 
/// The target clip space coordinate system is a left-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces up.
/// * The clip space **z-axis** faces the camera forward direction.
///
/// In particular, we map the eye space **z-axis** to the clip space **z-axis** by
/// changing the sign of the eye space **z coordinate**. The transformation maps 
/// the eye space frustum volume contained in `[left, right] x [bottom, top] x [-far, -near]` 
/// to the normalized device coordinates `[-1, -1] x [-1, 1] x [0, 1]`. The projection
/// matrix is given by
/// ```text
/// [ m[0, 0]  0         m[2, 0]  0       ]
/// [ 0        m[1, 1]   m[2, 1]  0       ]
/// [ 0        0         m[2, 2]  m[3, 2] ]
/// [ 0        0        -1        0       ]
/// ```
/// where
/// ```text
/// m[0, 0] ==  2 * near / (right - left)
/// m[2, 0] ==  (right + left) / (right - left)
/// m[1, 1] ==  2 * near / (top - bottom)
/// m[2, 1] ==  (top + bottom) / (top - bottom)
/// m[2, 2] == -far / (far - near)
/// m[3, 2] == -(far * near) / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// 
/// * `left` is the horizontal position of the left plane in eye space.
/// The left plane is a plane parallel to the **yz-plane** along the **x-axis**.
/// * `right` is the horizontal position of the right plane in eye space.
/// The right plane is a plane parallel to the **yz-plane** along the **x-axis**. 
/// * `bottom` is the vertical position of the bottom plane in eye space.
/// The bottom plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `top` is the vertical position of the top plane in eye space.
/// The top plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `near` is the distance along the **negative z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **negative z-axis**.
/// * `far` is the distance along the **negative z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **negative z-axis**.
/// 
/// The parameters must satisfy the following constraints to generate a valid 
/// perspective projection matrix.
/// ```text
/// left < right
/// bottom < top
/// 0 < near < far
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// #
/// let left = -4_f32;
/// let right = 4_f32;
/// let bottom = -3_f32;
/// let top = 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let expected = Matrix4x4::new(
///     1_f32 / 40_f32, 0_f32,           0_f32,               0_f32,
///     0_f32,          1_f32 / 30_f32,  0_f32,               0_f32,
///     0_f32,          0_f32,          -1000_f32 / 999_f32, -1_f32,
///     0_f32,          0_f32,          -100_f32 / 999_f32,   0_f32
/// );
/// let result = cglinalg_metal::perspective_frustum_rh(left, right, bottom, top, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
/// ```
pub fn perspective_frustum_rh<S>(left: S, right: S, bottom: S, top: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;

    let c0r0 = (two * near) / (right - left);
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = (two * near) / (top - bottom);
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 =  (right + left) / (right - left);
    let c2r1 =  (top + bottom) / (top - bottom);
    let c2r2 = -far / (far - near);
    let c2r3 = -one;

    let c3r0 =  zero;
    let c3r1 =  zero;
    let c3r2 = -(far * near) / (far - near);
    let c3r3 =  zero;
    
    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}

/// Construct a new perspective projection transformation mapping from 
/// an eye space with a left-handed coordinate system to a clip space with a 
/// left-handed coordinate coordinate system compatible with Metal's normalized 
/// device coordinates.
///
/// The source eye space coordinate system is a left-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces up.
/// * The camera space **positive z-axis** faces the forward direction.
/// 
/// The target clip space coordinate system is a left-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces up.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// The transformation maps the eye space volume 
/// `[left, right] x [bottom, top] x [near, far]` to the normalized device coordinates
/// `[-1, -1] x [-1, 1] x [0, 1]`. The projection matrix is given by
/// ```text
/// [ m[0, 0]  0        m[2, 0]  0       ]
/// [ 0        m[1, 1]  m[2, 1]  0       ]
/// [ 0        0        m[2, 2]  m[3, 2] ]
/// [ 0        0        1        0       ]
/// ```
/// where
/// ```text
/// m[0, 0] ==  2 * near / (right - left)
/// m[2, 0] == -(right + left) / (right - left)
/// m[1, 1] ==  2 * near / (top - bottom)
/// m[2, 1] == -(top + bottom) / (top - bottom)
/// m[2, 2] ==  far / (far - near)
/// m[3, 2] == -(far * near) / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// * `left` the horizontal position of the left plane in eye space.
/// The left plane is a plane parallel to the **yz-plane** along the **x-axis**.
/// * `right` the horizontal position of the right plane in eye space.
/// The right plane is a plane parallel to the **yz-plane** along the **x-axis**. 
/// * `bottom` the vertical position of the bottom plane in eye space.
/// The bottom plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `top` the vertical position of the top plane in eye space.
/// The top plane is a plane parallel to the **zx-plane** along the **y-axis**.
/// * `near` the distance along the **positive z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// * `far` the distance along the **positive z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
///
/// The parameters must satisfy the following constraints to generate a valid 
/// perspective projection matrix.
/// ```text
/// left < right
/// bottom < top
/// 0 < near < far
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// #
/// let left = -4_f32;
/// let right = 4_f32;
/// let bottom = -3_f32;
/// let top = 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let expected = Matrix4x4::new(
///     1_f32 / 40_f32, 0_f32,           0_f32,              0_f32,
///     0_f32,          1_f32 / 30_f32,  0_f32,              0_f32,
///     0_f32,          0_f32,           1000_f32 / 999_f32, 1_f32,
///     0_f32,          0_f32,          -100_f32 / 999_f32,  0_f32
/// );
/// let result = cglinalg_metal::perspective_frustum_lh(left, right, bottom, top, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
/// ```
pub fn perspective_frustum_lh<S>(left: S, right: S, bottom: S, top: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;

    let c0r0 = (two * near) / (right - left);
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = (two * near) / (top - bottom);
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 = -(right + left) / (right - left);
    let c2r1 = -(top + bottom) / (top - bottom);
    let c2r2 =  far / (far - near);
    let c2r3 =  one;

    let c3r0 =  zero;
    let c3r1 =  zero;
    let c3r2 = -(far * near) / (far - near);
    let c3r3 =  zero;
    
    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}
