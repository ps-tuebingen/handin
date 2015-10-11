update () {
  git checkout $1
  git pull
}

merge() {
  git checkout $1
  git merge $2 --ff-only
}

end() {
  git checkout master
}
