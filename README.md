# helm-shell-history.el

Sorry work in progress yet.

## Commentary:
This program is thing that it find shell history by helm.

## Commentary(Japanese):
このプログラムはhelm.elを使ってshell historyを表示するものです。

## how to install(Emacs 24 User)
You can install from M-x `list-packages' command.

## how to install(otherwise)
You can install that is used git:
    mkdir -p ~/.emacs.d/packages/
    cd ~/.emacs.d/packages/
    git clone git@github.com:yuutayamada/helm-shell-history.git

## configuration for Emacs
this is example for multi-term.el

   (require 'helm-shell-history)
   (add-hook 'term-mode-hook
             (lambda ()
               (define-key term-raw-map (kbd "C-r") 'helm-shell-history)))

## License
This is GNU General Public License. see <http://www.gnu.org/licenses/>
