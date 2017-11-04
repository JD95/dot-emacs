;;; purescript.el --- Configurations for purescript files

;;; Commentary:
;; 

(require 'psc-ide)

;;; Code:

(add-hook 'purescript-mode-hook
	  (lambda ()
	    (psc-ide-mode)
	    (company-mode)
	    (flycheck-mode)
	    (turn-on-purescript-indentation)
	    ))


(defun new-halogen-component ()
  "Generates a new purescript halogen component from template"
  (interactive)
  (let* ((name (read-string "component name: "))
	 (template `(,(concat "module Component." name "(State(..),Query(..)," (downcase name) ") where\n")
		     "\n"
		     "import CSS\n"
		     "import CSS.Elements as CSS\n"
		     "import CSS.Geometry as CSS\n"
		     "import CSS.Selector as CSS\n"
		     "import CSS.Stylesheet as CSS\n"
		     "import CSS.TextAlign as CSS\n"
		     "import CSS.Size as CSS\n"
		     "import Control.Monad.Aff (Aff)\n"
		     "import DOM.HTmL.Indexed as D\n"
		     "import Data.Maybe (Maybe(..))\n"
		     "import Halogen as H\n"
		     "import Halogen.HTML as HH\n"
		     "import Halogen.HTML.CSS as HC\n"
		     "import Halogen.HTML.Events as HE\n"
		     "import Halogen.HTML.Properties as HP\n"
		     "import Halogen.Themes.Bootstrap3 as HC\n"
		     "import Network.HTTP.Affjax as AX\n"
		     "import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($))\n"
		     "\n"
		     "import Types as T"
		     "\n"
		     "data State = State\n"
		     "\n"
		     "data Query a = Input a\n"
		     "\n"
		     "data ChildQuery a = ChildInput a\n"
		     "\n"
		     "type Slot = Int"
		     "\n"
		     "render :: forall eff. State -> H.ParentHTML Query ChildQuery Slot (Aff (T.Effect eff))\n"
		     "render st = HH.div_ []\n"
		     "\n"
		     "initialState :: State\n"
		     "initialState = State\n"
		     "\n"
		     "eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery Slot Void (Aff (T.Effect eff))\n"
		     "eval = case _ of\n"
		     "  Input next -> pure next\n"
		     "\n"
		     ,(concat (downcase name) " :: forall eff. H.Component HH.HTML Query Unit Void (Aff (T.Effect eff))\n")
		     ,(concat (downcase name) " =\n")
		     "  H.parentComponent\n"
		     "    { initialState: const initialState\n"
		     "    , render\n"
		     "    , eval\n"
		     "    , receiver: const Nothing\n"
		     "    }\n"
   		     )))
    (mapc 'insert template))
  )

(provide 'purescript)

;;; purescript.el ends here
