;;; templates-purescript.el --- A collection of templates for purescript programming

;;; Commentary:
;; 

;;; Code:

(defun template-new-purescript-halogen-component ()
  "Generate a new purescript halogen component."
  (interactive)
  (template-new-template
   ((name "name:"))
   `(,(concat "module Component." name "(State(..),Query(..)," (downcase name) ") where\n")
     ""
     "import CSS"
     "import CSS.Elements as CSS"
     "import CSS.Geometry as CSS"
     "import CSS.Selector as CSS"
     "import CSS.Stylesheet as CSS"
     "import CSS.TextAlign as CSS"
     "import CSS.Size as CSS"
     "import Control.Monad.Aff (Aff)"
     "import DOM.HTmL.Indexed as D"
     "import Data.Maybe (Maybe(..))"
     "import Halogen as H"
     "import Halogen.HTML as HH"
     "import Halogen.HTML.CSS as HC"
     "import Halogen.HTML.Events as HE"
     "import Halogen.HTML.Properties as HP"
     "import Halogen.Themes.Bootstrap3 as HC"
     "import Network.HTTP.Affjax as AX"
     "import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($))"
     ""
     "import Types as T"
     ""
     "data State = State"
     ""
     "data Query a = Input a"
     ""
     "data ChildQuery a = ChildInput a"
     ""
     "type Slot = Int"
     ""
     "render :: forall eff. State -> H.ParentHTML Query ChildQuery Slot (Aff (T.Effect eff))"
     "render st = HH.div_ []"
     ""
     "initialState :: State"
     "initialState = State"
     ""
     "eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery Slot Void (Aff (T.Effect eff))"
     "eval = case _ of"
     "  Input next -> pure next"
     ""
     ,(concat (downcase name) " :: forall eff. H.Component HH.HTML Query Unit Void (Aff (T.Effect eff))")
     ,(concat (downcase name) " =")
     "  H.parentComponent"
     "    { initialState: const initialState"
     "    , render"
     "    , eval"
     "    , receiver: const Nothing"
     "    }"))
  )

(provide 'templates-purescript)

;;; templates-purescript.el ends here
