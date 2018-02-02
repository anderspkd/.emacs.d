clean:
	find . -iname "*.elc" -delete

profile:
	emacs -Q -l ./etc/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file \"$(abspath init.el)\"))" \
	-f profile-dotemacs

.PHONY: clean profile
