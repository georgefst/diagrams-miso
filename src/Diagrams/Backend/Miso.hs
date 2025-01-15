{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
  -- UndecidableInstances needed for ghc < 707

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Miso
-- Copyright   :  (c) 2015 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Miso
  ( MisoSvg(..) -- rendering token
  , B
    -- for rendering options specific to Miso
  , Options(..)
  , sizeSpec
  , svgAttributes
  -- ,svgDefinitions, idPrefix, svgAttributes, generateDoctype
  , misoDia
  , renderElement
  , onMouseDown
  , onMouseDown'
  , onMouseUp
  , onMouseUp'
  , onMouseMove
  , onMouseMove'
  , htmlDiagram
  , DiaAttr(..)
  , mkDiaAttr
  ) where

import           Control.Lens hiding (children, transform, ( # ))
import           Control.Monad.Reader
import           Data.Aeson hiding (Options, Result)
import           Data.Bifunctor
import qualified Data.Map as M
import           Data.Tree
import           Diagrams.Core.Compile
import           Diagrams.Core.Types (Annotation (..))
import           Diagrams.Prelude hiding (height, width, Attribute, size, view, local, text, query)
import           Diagrams.TwoD.Adjust (adjustDia2D)
import           Diagrams.TwoD.Text (Text(..))
import           Miso hiding (P, Options, view, Result, onMouseDown, onMouseUp, Node)
import           Miso.String (MisoString, ms)

import           Graphics.Rendering.Miso (RenderM, renderForeign, renderForeignCustom, mkTransformMatrix)
import qualified Graphics.Rendering.Miso as R
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Miso.Svg (foreignObject_, transform_)

nodeSvg_ :: MisoString -> [Attribute action] -> [View action] -> View action
nodeSvg_ = flip (node SVG) Nothing

-- | @MisoSvg@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data MisoSvg = MisoSvg
  deriving (Show)

type B = MisoSvg

type instance V MisoSvg = V2
type instance N MisoSvg = Double

instance Semigroup (Render MisoSvg V2 Double) where
  Render r1 <> Render r2_ = Render $ mappend r1 r2_
instance Monoid (Render MisoSvg V2 Double) where
  mempty = Render mempty

instance Backend MisoSvg V2 Double where
  newtype Render  MisoSvg V2 Double = Render RenderM
  type    Result  MisoSvg V2 Double = R.Element
  data    Options MisoSvg V2 Double = MisoOptions
    { _size            :: SizeSpec V2 Double   -- ^ The requested size.
    , _svgAttributes   :: R.Attrs
                          -- ^ Attributes to apply to the entire svg element.
    }

  renderRTree :: MisoSvg -> Options MisoSvg V2 Double -> RTree MisoSvg V2 Double Annotation -> Result MisoSvg V2 Double
  renderRTree _ opts rt = R.Element "svg" attrs $ runReader (rtree rt) mempty
    where
      rtree :: RTree MisoSvg V2 Double Annotation -> RenderM
      rtree (Node n rs) = case n of
        RPrim p                 -> unRender $ render MisoSvg p
        RStyle sty              -> local (<> sty) r
        _                       -> r
        where
          r = foldMap rtree rs
      V2 w h = specToSize 100 . view sizeSpec $ opts
      attrs = M.fromList [ ("width", show w)
                       , ("height", show h) ]
              <> _svgAttributes opts

  adjustDia c opts d = ( sz, t <> reflectionY, d' ) where
    (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- | Lens onto the size of the options.
sizeSpec :: Lens' (Options MisoSvg V2 Double) (SizeSpec V2 Double)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svgAttributes field of the options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options MisoSvg V2 Double) R.Attrs
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

mkWidget :: Element act -> View act
mkWidget (Element name attrs children) =
  nodeSvg_ (ms name) attrs (map mkWidget children)
mkWidget (SvgText str) = text (ms str)
mkWidget (SvgHtml size@(V2 width height) as t h) =
    foreignObject_
        ( [ width_ $ ms width
          , height_ $ ms height
          , transform_ $ ms $ R.mkTransformMatrix $ t
            <> reflectionY
            <> translation ((fromIntegral <$> size) / (-2))
          -- TODO we could use this instead of doing translation via the matrix...
          -- any actual advantage?
          -- , x_ $ ms x
          -- , y_ $ ms y
          ]
            <> (fmap absurd <$> as)
        )
        [absurd <$> h]
mkWidget (CustomElement v) = absurd <$> v

unRender :: Render MisoSvg V2 Double -> RenderM
unRender (Render els) = els

instance Renderable (Path V2 Double) MisoSvg where
  render _ = Render . R.renderPath

instance Renderable (Text Double) MisoSvg where
  render _ = Render . R.renderText

instance Transformable HTMLPrimitive where
  -- TODO basically copied from `Text` instance...
  transform t p@HTMLPrimitive{transformation = tt} = p{transformation = t <> tt <> t'}
    where
      t' = scaling (1 / avgScale t)
instance Renderable HTMLPrimitive MisoSvg where
  render _ HTMLPrimitive{size, attrs, transformation, html} =
    Render $ renderForeign size attrs transformation html
data HTMLPrimitive = HTMLPrimitive
  { size :: V2 Word
  , attrs :: [Attribute Void]
  , transformation :: T2 Double
  , html :: View Void
  }
  deriving Typeable
type instance V HTMLPrimitive = V2
type instance N HTMLPrimitive = Double

data CustomPrimitive = CustomPrimitive
  { viewer :: [T2 Double] -> [Attribute Void] -> View Void
  , transforms :: [T2 Double]
  }
  deriving Typeable
instance Transformable CustomPrimitive where
  transform t p@CustomPrimitive{transforms = tt} = p{transforms = tt <> [t]}
instance Renderable CustomPrimitive MisoSvg where
  render _ (CustomPrimitive viewer transforms) = Render $ renderForeignCustom $ viewer transforms
type instance V CustomPrimitive = V2
type instance N CustomPrimitive = Double

htmlDiagram :: V2 Word -> [Attribute Void] -> View Void -> Diagram B
htmlDiagram size@(V2 width height) attrs html =
    -- TODO for some reason, despite `HTMLPrimitive` being reimplemented based on this `CustomPrimitive` stuff
    -- (which was originally just a way to experiment quickly downstream without recompiling this library),
    -- this version doesn't respond properly to transformations, e.g. `scale 0.5`
    -- and in Monpad, it doesn't work properly when we don't have `windowSize = V2 2000 1000`
    -- mkQD (Prim HTMLPrimitive{transformation = mempty, ..}) (getEnvelope r) (getTrace r) mempty mempty
    mkQD
        ( Prim
            CustomPrimitive
                { transforms = mempty
                , viewer = \ts as ->
                    foreignObject_
                        ( [ width_ $ ms width
                          , height_ $ ms height
                          , transform_
                                . ms
                                . mkTransformMatrix
                                $ foldl
                                    (\t tt -> t <> tt <> scaling (1 / avgScale t))
                                    mempty
                                    ts
                                    <> reflectionY
                                    <> translation ((fromIntegral <$> size) / (-2))
                          ]
                            <> as
                            <> attrs
                        )
                        [absurd <$> html]
                }
        )
        (getEnvelope r)
        (getTrace r)
        mempty
        mempty
  where
    -- TODO specify trace and envelope directly instead
    r :: Diagram B = rect (fromIntegral width) (fromIntegral height)

instance Default (Options MisoSvg V2 Double) where
  def = MisoOptions absolute mempty

mouseEventDecoder :: Decoder (Int, Int)
mouseEventDecoder =
  Decoder
    (withObject "event" $ \o -> liftA2 (,) (o .: "clientX") (o .: "clientY"))
    (DecodeTarget [])

mkDiaAttr ::
  (Monoid a) =>
  MisoString ->
  Decoder e ->
  (e -> P2 Double) ->
  (e -> a -> action) ->
  DiaAttr a action
mkDiaAttr event eventDecoder getPoint f =
  DiaAttr
    (\dia t ->
       on
         event
         eventDecoder
         (\e -> f e $ sample dia $ transform (inv t) $ getPoint e)
    )

query :: Monoid a => MisoString -> (a -> action) -> DiaAttr a action
query event f =
  mkDiaAttr
    event
    mouseEventDecoder
    (fmap fromIntegral . p2)
    (\_ ann -> f ann)

pos :: MisoString -> (P2 Double -> action) -> DiaAttr a action
pos event f = contramapDiaAttrAnn (const ()) $
  mkDiaAttr
    event
    mouseEventDecoder
    c
    (\e _ -> f $ c e)
  where c = fmap fromIntegral . p2

data DiaAttr a action =
  DiaAttr (QDiagram MisoSvg V2 Double a -> Transformation V2 Double -> Attribute action)
contramapDiaAttrAnn :: (b -> a) -> DiaAttr a action -> DiaAttr b action
contramapDiaAttrAnn f (DiaAttr x) = DiaAttr $ \d -> x $ f <$> d

onMouseDown :: (P2 Double -> action) -> DiaAttr a action
onMouseDown = pos "mousedown"

onMouseDown' :: Monoid a => (a -> action) -> DiaAttr a action
onMouseDown' = query "mousedown"

onMouseUp :: (P2 Double -> action) -> DiaAttr a action
onMouseUp = pos "mouseup"

onMouseUp' :: Monoid a => (a -> action) -> DiaAttr a action
onMouseUp' = query "mouseup"

onMouseMove :: (P2 Double -> action) -> DiaAttr a action
onMouseMove = pos "mousemove"

onMouseMove' :: Monoid a => (a -> action) -> DiaAttr a action
onMouseMove' = query "mousemove"

misoDia  :: Monoid' a => Options MisoSvg V2 Double -> QDiagram MisoSvg V2 Double a -> [DiaAttr a act] -> View act
misoDia opts dia diaAttrs =
  let (t, Element name attrs children) =
        second toMisoElement (renderDiaT MisoSvg opts dia)
  in mkWidget
       (Element name (map (\(DiaAttr f) -> f dia t) diaAttrs ++ attrs) children)

renderElement :: R.Element -> View act
renderElement e =
  let Element name attrs children = toMisoElement e
  in mkWidget (Element name attrs children)

toMisoAttrs :: M.Map String String -> [Attribute act]
toMisoAttrs = map (uncurry textProp . bimap ms ms) . M.toList

data Element action
  = Element String
            [Attribute action]
            [Element action]
  | SvgText String
  | SvgHtml
    (V2 Word)
    [Attribute Void]
    (T2 Double)
    (View Void)
  | CustomElement
    (View Void)

toMisoElement :: R.Element -> Element action
toMisoElement (R.Element name attrs children) =
  Element name (toMisoAttrs attrs) (map toMisoElement children)
toMisoElement (R.SvgText t) = SvgText t
toMisoElement (R.SvgHtml v attrs attrs' t h) = SvgHtml v (attrs <> toMisoAttrs attrs') t h
toMisoElement (R.CustomElement v) = CustomElement $ v toMisoAttrs
