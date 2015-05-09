{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Bootstrap where

import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Internal as TBI

nav :: BH.Html  -- ^ Inner HTML.
    -> BH.Html  -- ^ Resulting HTML.
nav = TBI.Parent "nav" "<nav" "</nav>"

-- Bootstrap attributes
dataToggle :: BH.AttributeValue  -- ^ Attribute value.
            -> BH.Attribute       -- ^ Resulting attribute.
dataToggle = TBI.customAttribute "data-toggle"

dataTarget :: BH.AttributeValue  -- ^ Attribute value.
            -> BH.Attribute       -- ^ Resulting attribute.
dataTarget = TBI.customAttribute "data-target"

ariaExpanded :: BH.AttributeValue  -- ^ Attribute value.
              -> BH.Attribute       -- ^ Resulting attribute.
ariaExpanded = TBI.customAttribute "aria-expanded"

ariaControls :: BH.AttributeValue  -- ^ Attribute value.
              -> BH.Attribute       -- ^ Resulting attribute.
ariaControls = TBI.customAttribute "aria-controls"

ariaHaspopup :: BH.AttributeValue  -- ^ Attribute value.
              -> BH.Attribute       -- ^ Resulting attribute.
ariaHaspopup = TBI.customAttribute "aria-haspopup"

ariaLabelledby :: BH.AttributeValue  -- ^ Attribute value.
                -> BH.Attribute       -- ^ Resulting attribute.
ariaLabelledby = TBI.customAttribute "aria-labelledby"

role :: BH.AttributeValue  -- ^ Attribute value.
     -> BH.Attribute       -- ^ Resulting attribute.
role = TBI.customAttribute "role"
