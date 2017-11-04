(defun template-new-python-sript ()
  (interactive)
  (template-new-template
   ()
   '("#!~/usr/bin/env python"
     ""
     "if __name__=='__main__':"
     "    pass"
     )))
