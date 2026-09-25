# ELPS internals

These are implementation and design notes for contributors and reviewers.
They are not user documentation: they describe how ELPS works inside and
why, and they can change without notice. For the language and embedding
guides, see the parent [docs](..) directory.

- [sealed-ast.md](sealed-ast.md): the `IsSealed` write protection for shared
  program literals, its verification layers, and the embedder contract.
- [walker-oracle.md](walker-oracle.md): the behavioral guard shared by every
  value-rebuilding walker.
- [tailrec-optimization.md](tailrec-optimization.md): how the interpreter
  collapses stack frames for tail-recursive calls.
- [linter-design.md](linter-design.md): the original design of `elps lint`.
- [marketplace-recovery.md](marketplace-recovery.md): diagnosing and
  recovering VS Code Marketplace publication of the editor extension.
- [template-vm-instantiation.md](template-vm-instantiation.md): how
  `Template.NewVM` became cheap: frozen packages, slot writes and lazy
  instantiation.
- [template-poc/](template-poc/README.md): archived experiments and
  benchmarks behind the [immutable VM template](../templates.md) design.
