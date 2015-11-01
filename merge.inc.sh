update () {
  git checkout $1
  git pull --ff-only
}

merge() {
  git checkout $1
  git merge $2 --ff-only
}

end() {
  git checkout master
}
