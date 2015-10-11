setup () {
  git checkout master
  git pull
}

merge() {
  git checkout deploy-$1
  git merge master --ff-only
}

end() {
  git checkout master
}
