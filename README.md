<p align="center"><img src="assets/emacs-logo.svg" width=150 height=150/></p>
<p align="center"><a href="https://www.gnu.org/software/emacs/"><b>GNU Emacs</b></a></p>
<p align="center">
	<a href="https://www.gnu.org/software/emacs/"><img src="https://img.shields.io/badge/GNU%20Emacs-28.0.50-b48ead.svg?style=flat-square"/></a>
	<a href="https://orgmode.org/"><img src="https://img.shields.io/badge/org--mode-9.4.6-489a9f.svg?style=flat-square"/></a>
	<a href="https://github.com/jwiegley/use-package"><img src="https://img.shields.io/badge/use--package-2.4.1-88c0d0.svg?style=flat-square"/></a>
</p>
<p align="center">This repository contains all my GNU Emacs configuration.</p>

---

I wanted to make this configuration simple, powerful and ergonomic by choosing
the most trivial keyboard possible.

<p align="center"><img src="assets/emacs-preview.png"/></p>

<blockquote>
	Font: <a href="https://adobe-fonts.github.io/source-code-pro">Source Code Pro</a> 20px <br>
    Mode Line: <a href="https://github.com/seagle0128/doom-modeline">DOOM Modeline</a> <br>
	Theme: <a href="https://github.com/arcticicestudio/nord-emacs">Nord Emacs</a>
</blockquote>

List of files:

* [`config.org`](https://github.com/rememberYou/.emacs.d/blob/master/config.org/):
  main configuration file.

* [`init.el`](https://github.com/rememberYou/.emacs.d/blob/master/init.el/):
  load the generated configuration file.

## Getting Started

If you are new to GNU Emacs, I advise you to take code blocks from the
[`config.org`](https://github.com/rememberYou/.emacs.d/blob/master/config.org/)
file for your own configuration file, and evaluate them with `eval-last-sepx`
(`C-x C-e`). Be careful to take pairs of parentheses.

For the most curious, you can test my complete configuration with:

	git clone --recursive https://github.com/rememberYou/.emacs.d

**NOTE:** the first time GNU Emacs starts, it will install additional packages
that will be better managed by the package manager.

## TIPs

You can see all the tips I wrote on Reddit sorted from the most recent to the
oldest according to the following links:
* [How to use a stable and fast environment to develop in Python](https://www.reddit.com/r/emacs/comments/ijmgtx/tip_how_to_use_a_stable_and_fast_environment_to/)
* [Packages to include in your workflow (Part II)](https://www.reddit.com/r/emacs/comments/awyvpa/tip_packages_to_include_in_your_workflow_part_ii/)
* [How to use a stable and fast environment to develop in C++](https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/)
* [How I organize myself with org-mode](https://www.reddit.com/r/emacs/comments/9ajxqj/tip_how_i_organize_myself_with_orgmode/)
* [How do I synchronize my org files with my devices](https://www.reddit.com/r/emacs/comments/98nkt6/tip_how_do_i_synchronize_my_org_files_with_my/)
* [How to get started with Git](https://www.reddit.com/r/emacs/comments/96r8us/tip_how_to_get_started_with_git/)
* [Packages to include in your workflow (Part I)](https://www.reddit.com/r/emacs/comments/94t6p6/tip_packages_to_include_in_your_workflow_part_i/)
* [How to adopt flycheck as your new best friend](https://www.reddit.com/r/emacs/comments/931la6/tip_how_to_adopt_flycheck_as_your_new_best_friend/)
* [How to use Ivy and its utilities in your workflow](https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/)
* [How to integrate company as completion framework](https://www.reddit.com/r/emacs/comments/8z4jcs/tip_how_to_integrate_company_as_completion/)
* [How I use ledger to track my money](https://www.reddit.com/r/emacs/comments/8x4xtt/tip_how_i_use_ledger_to_track_my_money/)
* [How to integrate your snippets with YASnippets](https://www.reddit.com/r/emacs/comments/8vdhb4/tip_how_to_integrate_snippets_with_yasnippets/)
* [How to manage your contacts with org-contacts](https://www.reddit.com/r/emacs/comments/8toivy/tip_how_to_manage_your_contacts_with_orgcontacts/)
* [How to better manage your spelling mistakes](https://www.reddit.com/r/emacs/comments/8rxm7h/tip_how_to_better_manage_your_spelling_mistakes/)
* [How to easily manage your emails with mu4e](https://www.reddit.com/r/emacs/comments/8q84dl/tip_how_to_easily_manage_your_emails_with_mu4e/)
* [How to be a beast with hydra](https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/)
* [How to make ERC fun to use](https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/)
* [How I use org-journal to improve my productivity](https://www.reddit.com/r/emacs/comments/8kz8dv/tip_how_i_use_orgjournal_to_improve_my/)
* [How to use your dashboard properly](https://www.reddit.com/r/emacs/comments/8jaflq/tip_how_to_use_your_dashboard_properly/)
* [How to speed up your Emacs config by 0.3 seconds](https://www.reddit.com/r/emacs/comments/8gbopk/tip_how_to_speed_up_your_emacs_config_by_03/)
* [How to execute a bash function when saving a specific file](https://www.reddit.com/r/emacs/comments/8hpyp5/tip_how_to_execute_a_bash_function_when_saving_a/)

## Contributions

Various functions may be optimized or spelling errors may occur. If you want to
make your own correction on this configuration, you are free to do so in the
[issue tracker](https://github.com/rememberYou/.emacs.d/issues).

## License

The code is not licensed, take what you like and hope that this configuration
can be so useful to you that it is for me.

GNU Emacs is above all a concept of sharing in order to facilitate our daily life.
