#![doc = include_str!("../README.md")]
#![no_std]

#[cfg(feature = "core")]
extern crate core;

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

extern crate cglinalg;

use cglinalg::{
    Angle,
    Matrix4x4,
    Radians,
    SimdScalarFloat,
};

/// Construct a new canonical perspective field of view projection 
/// transformation mapping from an eye space with a right-handed 
/// coordinate system to a clip space with a right-handed coordinate 
/// system compatible with Vulkan's normalized device coordinates.
///
/// The source eye space coordinate system is a right-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces down.
/// * The camera space forward direction faces the **positive z-axis**.
/// * The camera space **z-axis** faces the positive forward direction.
/// 
/// The target clip space coordinate system is a right-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces down.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// This variant of the perspective projection matrix is parametrized using 
/// the vertical field of view angle `vfov` and the aspect ratio `aspect_ratio`. 
/// The aspect ratio `aspect_ratio` is the ratio of the width of the camera 
/// viewport to the height of the camera viewport.
/// 
/// We take the coordinate system for the view space and clip spaces above
/// to be the **canonical view space coordinate system** and the 
/// **canonical clip  space coordinate system** for Vulkan. The canonical 
/// coordinate systems are specially chosen ones as defined by the API specification
/// for the platform API. Here
/// 
/// ```text
/// tan(vfov) == bottom / near
/// aspect_ratio == right / bottom
/// ```
/// 
/// The transformation maps the eye space frustum volume contained in 
/// `[-right, right] x [-bottom, bottom] x [near, far]` to the clip space that 
/// maps to the normalized device coordinates `[-1, -1] x [-1, 1] x [0, 1]` 
/// under division by the `w-component`.
/// 
/// The projection matrix is given by
/// 
/// ```text
/// [ m[0, 0]  0         0        0       ]
/// [ 0        m[1, 1]   0        0       ]
/// [ 0        0         m[2, 2]  m[3, 2] ]
/// [ 0        0        -1        0       ]
/// ```
/// 
/// where
/// 
/// ```text
/// m[0, 0] ==  1 / (aspect_ratio * tan(vfov / 2))
/// m[1, 1] ==  1 / tan(vfov / 2)
/// m[2, 2] == -far / (far - near)
/// m[3, 2] == -(far * near) / (far - near)
/// ```
/// 
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// 
/// * `vfov` is the vertical angle in radians of the symmetric viewing frustum
/// centered at the eye position in the **xy-plane**.
/// * `aspect_ratio` is the ratio of the width of the symmetric viewing frustum to the
/// height of the symmetric viewing frustum.
/// * `near` is the distance along the **negative z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// * `far` the distance along the **positive z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
///
/// The parameters must satisfy the following constraints to generate a valid 
/// perspective projection matrix.
/// 
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
///     0_f32, 0_f32,  1000_f32 / 999_f32,  1_f32,
///     0_f32, 0_f32, -100_f32 / 999_f32,   0_f32
/// );
/// let result = cglinalg_vulkan::perspective_fov(vfov, aspect_ratio, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-6, relative_all <= f32::EPSILON);
/// ```
#[rustfmt::skip]
#[inline]
pub fn perspective_fov<S, A>(vfov: A, aspect_ratio: S, near: S, far: S) -> Matrix4x4<S>
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

/// Construct a new canonical perspective projection transformation mapping from 
/// an eye space with a right-handed coordinate system to a clip space with a 
/// right-handed coordinate coordinate system compatible with Vulkan's normalized 
/// device coordinates.
///
/// The source eye space coordinate system is a right-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces down.
/// * The camera space forward direction faces the **positive z-axis**.
/// * The camera space **z-axis** faces the positive forward direction.
/// 
/// The target clip space coordinate system is a right-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces down.
/// * The clip space **z-axis** faces the camera forward direction.
///
/// The transformation maps the eye space frustum volume contained in 
/// `[left, right] x [top, bottom] x [near, far]` to the clip space that 
/// maps to the normalized device coordinates `[-1, -1] x [-1, 1] x [0, 1]` under
/// division by the **w-component**. 
/// 
/// We take the coordinate system for the view space and clip spaces above
/// to be the **canonical view space coordinate system** and the **canonical clip 
/// space coordinate system** for Vulkan. The canonical coordinate systems are 
/// specially chosen ones as defined by the API specification for the platform API.
/// 
/// The projection matrix is given by
/// 
/// ```text
/// [ m[0, 0]  0         m[2, 0]  0       ]
/// [ 0        m[1, 1]   m[2, 1]  0       ]
/// [ 0        0         m[2, 2]  m[3, 2] ]
/// [ 0        0         1        0       ]
/// ```
/// 
/// where
/// 
/// ```text
/// m[0, 0] ==  2 * near / (right - (-left))
/// m[2, 0] == -(right + (-left)) / (right - (-left))
/// m[1, 1] ==  2 * near / (bottom - (-top))
/// m[2, 1] == -(bottom + (-top)) / (bottom - (-top))
/// m[2, 2] ==  far / (far - near)
/// m[3, 2] == -(far * near) / (far - near)
/// ```
/// 
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// 
/// * `left` is the horizontal position of the left plane in eye space.
/// The left plane is a plane parallel to the **yz-plane** along the **positive x-axis**.
/// * `right` is the horizontal position of the right plane in eye space.
/// The right plane is a plane parallel to the **yz-plane** along the **positive x-axis**. 
/// * `bottom` is the vertical position of the bottom plane in eye space.
/// The bottom plane is a plane parallel to the **zx-plane** along the **positive y-axis**.
/// * `top` is the vertical position of the top plane in eye space.
/// The top plane is a plane parallel to the **zx-plane** along the **positive y-axis**.
/// * `near` is the distance along the **negative z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// * `far` is the distance along the **negative z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// 
/// The parameters must satisfy the following constraints to generate a valid 
/// perspective projection matrix.
/// 
/// ```text
/// left       > 0
/// right      > 0
/// bottom     > 0
/// top        > 0
/// far > near > 0
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// #
/// let left = 4_f32;
/// let right = 4_f32;
/// let bottom = 3_f32;
/// let top = 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let expected = Matrix4x4::new(
///     1_f32 / 40_f32, 0_f32,           0_f32,               0_f32,
///     0_f32,          1_f32 / 30_f32,  0_f32,               0_f32,
///     0_f32,          0_f32,           1000_f32 / 999_f32,  1_f32,
///     0_f32,          0_f32,          -100_f32 / 999_f32,   0_f32
/// );
/// let result = cglinalg_vulkan::perspective_frustum(left, right, bottom, top, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
/// ```
#[rustfmt::skip]
#[inline]
pub fn perspective_frustum<S>(left: S, right: S, bottom: S, top: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;

    let c0r0 = (two * near) / (right - (-left));
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = (two * near) / (bottom - (-top));
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 = -(right + (-left)) / (right - (-left));
    let c2r1 = -(bottom + (-top)) / (bottom - (-top));
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

/// Construct a new canoical orthographic projection transformation mapping from 
/// an eye space with a right-handed coordinate system to a clip space with a 
/// right-handed coordinate coordinate system compatible with Vulkan's normalized 
/// device coordinates.
///
/// The source eye space coordinate system is a right-handed coordinate 
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The camera space **x-axis** faces right.
/// * The camera space **y-axis** faces down.
/// * The camera space forward direction faces the **positive z-axis**.
/// * The camera space **z-axis** faces the negative forward direction.
/// 
/// The target clip space coordinate system is a right-handed coordinate
/// system with `(x, y, z)` coordinates with the following properties:
/// 
/// * The clip space **x-axis** faces right.
/// * The clip space **y-axis** faces down.
/// * The clip space **z-axis** faces the camera forward direction.
/// 
/// The transformation maps the eye space volume 
/// `[left, right] x [bottom, top] x [-far, -near]` to the clip space that maps 
/// to the normalized device coordinates `[-1, -1] x [-1, 1] x [0, 1]` under
/// division by the **w-component**.
/// 
/// We take the coordinate system for the view space adn clip spaces above
/// to be the **canonical view space coordinate system** and the **canonical clip
/// space coordinate system** for Vulkan. The canonical coordinate systems are 
/// specially chosen one as defined by the API specificaion for the platform API.
/// 
/// The projection matrix is given by
/// 
/// ```text
/// [ m[0, 0]  0        0        m[3, 0] ]
/// [ 0        m[1, 1]  0        m[3, 1] ]
/// [ 0        0        m[2, 2]  m[3, 2] ]
/// [ 0        0        0        1       ]
/// ```
/// 
/// where
/// 
/// ```text
/// m[0, 0] ==  2 / (right - (-left))
/// m[3, 0] == -(right + (-left)) / (right - (-left))
/// m[1, 1] ==  2 / (bottom - (-top))
/// m[3, 1] == -(bottom + (-top)) / (bottom - (-top))
/// m[2, 2] ==  1 / (far - near)
/// m[3, 2] == -near / (far - near)
/// ```
/// where the matrix entries are indexed in column-major order.
/// 
/// # Parameters
/// 
/// * `left` is the horizontal position of the left plane in eye space.
/// The left plane is a plane parallel to the **yz-plane** along the **positive x-axis**.
/// * `right` is the horizontal position of the right plane in eye space.
/// The right plane is a plane parallel to the **yz-plane** along the **positive x-axis**.
/// * `bottom` is the vertical position of the bottom plane in eye space.
/// The bottom plane is a plane parallel to the **zx-plane** along the **positive y-axis**.
/// * `top` is the vertical position of the top plane in eye space.
/// The top plane is a plane parallel to the **zx-plane** along the **positive y-axis**.
/// * `near` is the distance along the **negative z-axis** of the near plane from the
/// eye in eye space. The near plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// * `far` is the distance along the **negative z-axis** of the far plane from the
/// eye in eye space. The far plane is a plane parallel to the **xy-plane** along
/// the **positive z-axis**.
/// 
/// The parameters must satisfy the following constraints to generate a valid 
/// orthographic projection matrix.
/// 
/// ```text
/// left       > 0
/// right      > 0
/// bottom     > 0
/// top        > 0
/// far > near > 0
/// ```
/// 
/// # Example
/// 
/// ```
/// # use approx_cmp::assert_relative_eq;
/// # use cglinalg::Matrix4x4;
/// #
/// let left = 4_f32;
/// let right = 4_f32;
/// let bottom = 3_f32;
/// let top = 3_f32;
/// let near = 0.1_f32;
/// let far = 100_f32;
/// let expected = Matrix4x4::new(
///     1_f32 / 4_f32, 0_f32,          0_f32,            0_f32,
///     0_f32,         1_f32 / 3_f32,  0_f32,            0_f32,
///     0_f32,         0_f32,          10_f32 / 999_f32, 0_f32,
///     0_f32,         0_f32,         -1_f32 / 999_f32,  1_f32
/// );
/// let result = cglinalg_vulkan::orthographic_frustum(left, right, bottom, top, near, far);
/// 
/// assert_relative_eq!(result, expected, abs_diff_all <= 1e-7, relative_all <= f32::EPSILON);
/// ```
#[rustfmt::skip]
#[inline]
pub fn orthographic_frustum<S>(left: S, right: S, bottom: S, top: S, near: S, far: S) -> Matrix4x4<S>
where
    S: SimdScalarFloat,
{
    let zero = S::zero();
    let one = S::one();
    let two = one + one;

    let c0r0 = two / (right - (-left));
    let c0r1 = zero;
    let c0r2 = zero;
    let c0r3 = zero;

    let c1r0 = zero;
    let c1r1 = two / (bottom - (-top));
    let c1r2 = zero;
    let c1r3 = zero;

    let c2r0 = zero;
    let c2r1 = zero;
    let c2r2 = one / (far - near);
    let c2r3 = zero;

    let c3r0 = -(right + (-left)) / (right - (-left));
    let c3r1 = -(bottom + (-top)) / (bottom - (-top));
    let c3r2 = -near / (far - near);
    let c3r3 =  one;

    Matrix4x4::new(
        c0r0, c0r1, c0r2, c0r3,
        c1r0, c1r1, c1r2, c1r3,
        c2r0, c2r1, c2r2, c2r3,
        c3r0, c3r1, c3r2, c3r3
    )
}
