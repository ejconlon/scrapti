stack_build := "stack build --fast"
src_dirs := "src test exe sfmerge"

# No default tasks
default:
  just --list

# Build and run tests
test:
  {{ stack_build }} --test

# Build only
build:
  {{ stack_build }} --test --no-run-tests

# Clean stack work
clean:
  stack clean --full

# Enter repl
ghci:
  stack ghci --test

# Open browser with generated docs
docs:
  stack haddock --open

# Install tool deps
deps:
  stack build --copy-compiler-tool hlint fourmolu apply-refact

# Format with fourmolu
format:
  stack exec -- fourmolu --mode inplace {{ src_dirs }}

# Lint with hlint
lint:
  stack exec -- hlint {{ src_dirs }}

# Apply hlint suggestions
lint-apply:
  find {{ src_dirs }} -name '*.hs' | xargs -t -I % stack exec -- hlint % --refactor --refactor-options="--inplace"

# Run the executable
exe:
  {{ stack_build }} --test --no-run-tests --exec scrapti-exe
