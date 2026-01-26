# How to release `orville-postgresql` to Hackage

## Move to the `orville-postgresql` directory

```sh
cd <orville project root>/orville-postgresql
```

## Create/checkout a release branch

If this minor version release, checkout the existing release branch

```sh
git checkout release/x.y # E.G. release/1.1
```

If this is a major version release, then create a new release branch for the major version

```sh
git checkout -b release/x.y # E.G. release/2.1
```

## Set the package version

Edit the `package.yaml` file to set the package version:

```yaml
version: 'x.y.z.w'
```

And then run

```sh
./scripts/test
```

To get the cabal file updated.

Commit this and push it to the release branch.

## Create a release candidate on Hackage

Use the release script to prepare and upload a candidate

```sh
./scripts/release prepare-candidate
./scripts/release upload-candidate x.y.z.w
```

## Check over the candidate on Hackage

Open up `https://hackage.haskell.org/package/orville-postgresql-x.y.z.w/candidate` in your
browser and give it a look over. Check that the docs look ok, especially for anything that
was added.

If anything needs to be changed, make the changes to the release branch and upload a new
candidate until you're satisfied.

## Publish the candidate on Hackage

Use the release script to publish the candidate. This will publish the docs along with
it, which using the publish button on Hackage will not do.

```sh
./scripts/release publish-candidate x.y.z.w
```

## Tag and create a github release

Run these commands locally to create a new tag and push it to github

```sh
git tag x.y.z.w
git push origin x.y.z.w
```

Then go to github and create a release by visiting

https://github.com/flipstone/orville/tags

Open the `...` menu on the tag you just created. Select `New Release`

- Release name: `Releaase x.y.z.w`
- Release notes:
  - Use the `Generate Release Notes` button and then edit as desired
  - Add `**Full Docs**: https://hackage-content.haskell.org/package/orville-postgresql-x.y.z.w` at the end

Then save the release.

## Merge the release branch back to `main`

```sh
git checkout main
git pull
git merge release/x.y
```

If you get a merge conflict on the release number, update the release number to
`x.y.z.w.0` to start a new development version. If you _don't_ get a merge conflict,
then update the version number in a new commit.

Resolve any other merge conflicts as well.

Run `./scripts/build` to ensure your merge conflicts are resolved with all tests passing.

Push the merged branch to main.
