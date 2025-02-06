## Purpose of this PR

## Checklist:

- [ ] I used ./test-standard-runs to compare and archive the changes introduced by this PR in /p/projects/edget/PRchangeLog/

Instructions for PIK internal testing of transport packages:

  1.  Navigate to this folder on the cluster `/p/projects/edget/PRchangeLog/`
  2.  Run the following command in the folder: `./test-standard-runs "FolderName" "ReferenceFolder"`, where "FolderName" is a name you can choose which identifies your changes or PR, i.e. the PR number plus a keyword. "ReferenceFolder" is an optional argument and should be a folder that already exists in the directory "/p/projects/edget/PRchangeLog/". If not set this defaults to the last stored run. 
      Running this command submits four sbatch jobs. You can optionally find a help page by running ./test-standard-runs -h
  3.  You are now interactively asked if repositories should be installed from source. Here, name one after another all the repositories in which you introduced changes. You can for example use your GitHub branch for that.
  4.  Wait until the submitted jobs have finished and check if the folders "ChangeLogMix[1-4]" have been generated and hold a compScen each.  You can check the `testScen[1-4]_edgetPR*.out` files in case of errors.
  5.  Check the compScen if you wish to review changes of the results compared to the reference run.


## Further information (optional):

* Test runs are here: 
* Comparison of results (what changes by this PR?): 

