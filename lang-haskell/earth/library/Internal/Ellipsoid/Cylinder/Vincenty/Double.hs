module Internal.Ellipsoid.Cylinder.Vincenty.Double
    ( circumSample
    , direct
    , directUnchecked
    , cos2
    ) where

import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (u, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.DegMinSec (DMS(..))
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone
    ( Zone(..)
    , QRadius
    , Radius(..)
    , Bearing(..)
    , ArcSweep(..)
    , center
    , radius
    , realToFracZone
    , realToFracLatLng
    , fromRationalLatLng
    , toRationalLatLng
    )
import Flight.Zone.Path (distancePointToPoint)
import qualified Internal.Ellipsoid.PointToPoint.Double as E (distance)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , Tolerance(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    , sampleAngles
    )
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), GeodeticAccuracy(..), GeodeticDirect(..)
    , defaultGeodeticAccuracy, wgs84, flattening, polarRadius
    )
import Flight.Geodesy (EarthMath(..), DirectProblem(..), DirectSolution(..))
import Flight.Earth.ZoneShape.Double (PointOnRadial, onLine)
import Flight.Earth.Math (cos2)

iterateAngularDistance
    :: (Floating a, Ord a)
    => GeodeticAccuracy a -> a -> a -> a -> a -> a -> a -> a
iterateAngularDistance
    accuracy@(GeodeticAccuracy tolerance)
    _A
    _B
    s
    b
    σ1
    σ =
    if abs (σ - σ') < tolerance
        then σ
        else
            iterateAngularDistance accuracy _A _B s b σ1 σ'
    where
        (cos2σm, cos²2σm) = cos2 cos σ1 σ
        sinσ = sin σ
        cosσ = cos σ
        sin²σ = sinσ * sinσ

        _Δσ =
            _B * sinσ *
                (cos2σm + _B / 4 *
                    (cosσ * (-1 + 2 * cos²2σm)
                    - _B / 6
                    * cos2σm
                    * (-3 + 4 * sin²σ)
                    * (-3 + 4 * cos²2σm)
                    )
                )

        σ' = s / (b * _A) + _Δσ

-- | The solution to the direct geodesy problem with input latitude rejected
-- outside the range -90° .. 90° and longitude normalized to -180° .. 180°.
direct
    :: (Real a, Floating a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> GeodeticAccuracy a
    -> DirectProblem
        (LatLng a [u| rad |])
        (TrueCourse a)
        (QRadius a [u| m |])
    -> GeodeticDirect
        (DirectSolution
            (LatLng a [u| rad |])
            (TrueCourse a)
        )
direct
    e
    a
    p@DirectProblem
        { x = x@(LatLng (Lat qLat, _))
        , α₁ = TrueCourse qTC
        } =
    fromMaybe (error msg) $ do
        let LatLng (Lat xLat, Lng xLng) = toRationalLatLng x

        nLat <- plusMinusHalfPi xLat
        let nLng = plusMinusPi xLng
        let xNorm = fromRationalLatLng $ LatLng (Lat nLat, Lng nLng)

        let nTC = normalize $ toRational' qTC
        let tcNorm = TrueCourse . fromRational' $ nTC

        return $ directUnchecked e a p{x = xNorm, α₁ = tcNorm}
    where
        dmsLat :: DMS
        dmsLat = fromQuantity . fromRational' . toRational' $ qLat

        msg = printf "Latitude of %s is outside -90° .. 90° range" $ show dmsLat

-- | The solution to the direct geodesy problem with input latitude unchecked
-- and longitude not normalized.
directUnchecked
    :: (Real a, Floating a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> GeodeticAccuracy a
    -> DirectProblem
        (LatLng a [u| rad |])
        (TrueCourse a)
        (QRadius a [u| m |])
    -> GeodeticDirect
        (DirectSolution
            (LatLng a [u| rad |])
            (TrueCourse a)
        )
directUnchecked
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    accuracy
    DirectProblem
        { x = LatLng (Lat (MkQuantity _Φ1), Lng (MkQuantity λ1))
        , α₁ = TrueCourse (MkQuantity α1)
        , s = (Radius (MkQuantity s))
        } =
    GeodeticDirect $
        DirectSolution
            { y = LatLng (Lat . MkQuantity $ _Φ2, Lng . MkQuantity $ _L2)
            , α₂ = Just . TrueCourse . MkQuantity $ atan2 sinα j'
            }
    where
        MkQuantity b = polarRadius ellipsoid
        f = flattening ellipsoid

        -- Initial setup
        _U1 = atan $ (1 - f) * tan _Φ1
        cosU1 = cos _U1
        sinU1 = sin _U1

        cosα1 = cos α1
        sinα1 = sin α1
        σ1 = atan2 (tan _U1) (cos α1)

        sinα = cos _U1 * sin α1
        sin²α = sinα * sinα
        cos²α = 1 - sin²α

        u² =
            let a² = a * a
                b² = b * b
            in cos²α * (a² - b²) / b²

        _A = 1 + u² / 16384 * (4096 + u² * (-768 + u² * (320 - 175 * u²)))
        _B = u² / 1024 * (256 + u² * (-128 + u² * (74 - 47 * u²)))

        -- Solution
        σ = iterateAngularDistance accuracy _A _B s b σ1 (s / (b * _A))

        sinσ = sin σ
        cosσ = cos σ

        v = sinU1 * cosσ + cosU1 * sinσ * cosα1

        (j, j') =
            let sinU1sinσ = sinU1 * sinσ
                cosU1cosσcosα1 = cosU1 * cosσ * cosα1
            in
                (   sinU1sinσ  - cosU1cosσcosα1
                , -(sinU1sinσ) + cosU1cosσcosα1
                )

        w = (1 - f) * sqrt (sin²α + j * j)
        _Φ2 = atan2 v w
        λ = atan2 (sinσ * sinα1) (cosU1 * cosσ - sinU1 * sinσ * cosα1)
        _C = f / 16 * cos²α * (4 + f * (4 - 3 * cos²α))

        _L =
            let (cos2σm, cos²2σm) = cos2 cos σ1 σ
                y = cos2σm + _C * cosσ * (-1 + 2 * cos²2σm)
                x = σ + _C * sinσ * y
            in λ * (1 - _C) * f * sinα * x

        _L2 = _L + λ1

-- |
-- TODO: Why is a point 286 m out is 0.254 km away when using Vincenty direct
-- instead of Haversines direct solution.
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 286.27334927563106 m |]) [u| 332.30076790172313 deg |]
-- (-32.46135051411138°, 148.98758167886174°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| -32.46363 deg |], Lng $ convert [u| 148.989 deg |]))
--         (LatLng (Lat $ convert [u| -32.46135051411138 deg |], Lng $ convert [u| 148.98758167886174 deg |]))
-- :}
-- [u| 0.285797530532 km |]
--
-- TODO: Why is a point 177 m out is 0.157 km away.
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 177.23328234645362 m |]) [u| 152.30076790172313 deg |]
-- (-32.46504123330422°, 148.9898781257936°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| -32.46363 deg |], Lng $ convert [u| 148.989 deg |]))
--         (LatLng (Lat $ convert [u| -32.46504123330422 deg |], Lng $ convert [u| 148.9898781257936 deg |]))
-- :}
-- [u| 0.176938752465 km |]
--
-- TODO: Why is a point 40 m out is 0 km away when bearing 90° from (-45°, 0°).
-- >>> circumDeg (LatLng (Lat [u| -45.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 90 deg |]
-- (-44.99999999887073°, 5.087331248034837e-4°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| -45.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| -44.99999999887073 deg |], Lng $ convert [u| 5.087331248034837e-4 deg |]))
-- :}
-- [u| 4.0111996609e-2 km |]
--
-- TODO: Why is a point 40 m out is 0 km away when bearing 90° from (+45°, 0°).
-- >>> circumDeg (LatLng (Lat [u| 45.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 90 deg |]
-- (44.99999999887073°, 5.087331248034837e-4°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| 45.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| 44.99999999887073 deg |], Lng $ convert [u| 5.087331248034837e-4 deg |]))
-- :}
-- [u| 4.0111996609e-2 km |]
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 0 deg |]
-- (3.5972864236749223e-4°, 0.0°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| 0.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| 3.5972864236749223e-4 deg |], Lng $ convert [u| 0.0 deg |]))
-- :}
-- [u| 3.9776734122e-2 km |]
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 90 deg |]
-- (2.2027026521703902e-20°, 3.5972864236749223e-4°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| 0.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| 2.2027026521703902e-20 deg |], Lng $ convert [u| 3.5972864236749223e-4 deg |]))
-- :}
-- [u| 4.0044807783e-2 km |]
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| -90 deg |]
-- (2.2027026521703902e-20°, -3.5972864236749223e-4°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| 0.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| 2.2027026521703902e-20 deg |], Lng $ convert [u| -3.5972864236749223e-4 deg |]))
-- :}
-- [u| 4.0044807783e-2 km |]
__circum
    :: (Real a, Fractional a, RealFloat a)
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
__circum x r tc =
    case direct wgs84 accuracy' prob of
        GeodeticDirectAbnormal _ -> error "Geodetic direct abnormal"
        GeodeticDirectEquatorial -> error "Geodetic direct equatorial"
        GeodeticDirectAntipodal -> error "Geodetic direct antipodal"
        GeodeticDirect DirectSolution{y} -> realToFracLatLng y
    where
        GeodeticAccuracy accuracy = defaultGeodeticAccuracy
        accuracy' = GeodeticAccuracy $ fromRational accuracy

        prob =
            DirectProblem
                { x = x
                , α₁ = tc
                , s = r
                }

-- TODO: Find out why I'm getting reasonable hits using Haversines but not for
-- Vincenty for the direct solution.
circum
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum
    (LatLng (Lat (MkQuantity lat), Lng (MkQuantity lng)))
    (Radius (MkQuantity r))
    (TrueCourse (MkQuantity tc)) =
    LatLng (Lat (MkQuantity φ2), Lng (MkQuantity λ2))
    where
        φ1 = realToFrac lat
        λ1 = realToFrac lng
        θ = realToFrac tc

        δ = let Radius (MkQuantity bigR) = earthRadius in realToFrac r / bigR

        φ2 = asin $ sin φ1 * cos δ + cos φ1 * sin δ * cos θ
        λ2 = λ1 + atan2 (sin θ * sin δ * cos φ1) (cos δ - sin φ1 * sin φ2)


-- | Generates a pair of lists, the lat/lng of each generated point and its
-- distance from the center. It will generate 'samples' number of such points
-- that should lie close to the circle bounding the zone. The difference
-- between the distance to the origin and the radius should be less than the
-- 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Double
circumSample sp@SampleParams{..} arcSweep@(ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN
    | bearing < 0 || bearing > 2 * pi = fail "Arc sweep must be in the range 0..2π radians."
    | otherwise =
        case (zoneM, zoneN) of
            (Nothing, _) -> ys
            (Just _, Point{}) -> ys
            (Just _, Vector{}) -> ys
            (Just _, Cylinder{}) -> ys
            (Just _, Conical{}) -> ys
            (Just _, Line{}) -> onLine mkLinePt θ ys
            (Just _, Circle{}) -> ys
            (Just _, SemiCircle{}) -> ys
    where
        zone' :: Zone Double
        zone' = maybe zoneN sourceZone arc0

        (θ, xs) = sampleAngles pi sp arcSweep arc0 zoneM zoneN

        r :: QRadius Double [u| m |]
        r@(Radius (MkQuantity limitRadius)) = radius zone'

        ptCenter = center zone'
        circumR = circum ptCenter

        getClose' = getClose zone' ptCenter limitRadius spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b

        ys :: ([ZonePoint Double], [TrueCourse Double])
        ys = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

getClose
    :: Zone Double
    -> LatLng Double [u| rad |] -- ^ The center point.
    -> Double -- ^ The limit radius.
    -> Tolerance Double
    -> Int -- ^ How many tries.
    -> QRadius Double [u| m |] -- ^ How far from the center.
    -> (TrueCourse Double -> LatLng Double [u| rad |]) -- ^ A point from the origin on this radial
    -> TrueCourse Double -- ^ The true course for this radial.
    -> (ZonePoint Double, TrueCourse Double)
getClose zone' ptCenter limitRadius spTolerance trys (Radius (MkQuantity offset)) f x@(TrueCourse tc)
    | trys <= 0 = (zp', x)
    | unTolerance spTolerance <= 0 = (zp', x)
    | limitRadius <= unTolerance spTolerance = (zp', x)
    | otherwise =
        case d `compare` limitRadius of
             EQ ->
                 (zp', x)

             GT ->
                 let offset' =
                         offset - (d - limitRadius) * 105 / 100

                     f' =
                         circumR (Radius (MkQuantity $ limitRadius + offset'))

                 in
                     getClose
                         zone'
                         ptCenter
                         limitRadius
                         spTolerance
                         (trys - 1)
                         (Radius (MkQuantity offset'))
                         f'
                         x

             LT ->
                 if d > (limitRadius - unTolerance spTolerance)
                 then (zp', x)
                 else
                     let offset' =
                             offset + (limitRadius - d) * 94 / 100

                         f' =
                             circumR (Radius (MkQuantity $ limitRadius + offset'))

                     in
                         getClose
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (Radius (MkQuantity offset'))
                             f'
                             x
    where
        circumR = circum ptCenter

        y = f x

        zp' :: ZonePoint Double
        zp' = ZonePoint
                { sourceZone = realToFracZone zone'
                , point = y
                , radial = Bearing $ normalize tc
                , orbit = Radius yr
                } :: ZonePoint Double

        pts = [Point ptCenter, Point y]

        (TaskDistance yr@(MkQuantity d)) =
            edgesSum
            $ distancePointToPoint
                (E.distance Vincenty wgs84)
                (realToFracZone <$> pts)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :set -fno-warn-partial-type-signatures
--
-- >>> import Data.UnitsOfMeasure ((*:), u, convert)
-- >>> import Flight.LatLng (radToDegLL, degToRadLL)
-- >>> import Internal.Ellipsoid.PointToPoint.Vincenty.Double as V
--
-- >>> :{
-- circumDeg
--    :: RealFloat a
--    => LatLng a [u| deg |]
--    -> QRadius a [u| m |]
--    -> (Quantity a [u| deg |])
--    -> LatLng Double [u| deg |]
-- circumDeg ll r tc =
--     radToDegLL convert $ circum (degToRadLL convert ll) r (TrueCourse ((convert tc) :: Quantity _ [u| rad |]))
-- :}
