About helm-shell-history
=================

Overview
------------
Find shell history by helm

<!-- ## how to install(Emacs 24 User) -->
<!-- You can install from M-x `list-packages' command. -->

<!-- ## how to install(otherwise) -->
## Installation
You can install that is used git:
    mkdir -p ~/.emacs.d/packages/
    cd ~/.emacs.d/packages/
    git clone https://github.com/emacsmirror/helm-shell-history.git
## Configuration for Emacs
below code is example for multi-term.el

   (require 'helm-shell-history)
   (add-hook 'term-mode-hook
             (lambda ()
               (define-key term-raw-map (kbd "C-r") 'helm-shell-history)))

## License
This is GNU General Public License. see <http://www.gnu.org/licenses/>
