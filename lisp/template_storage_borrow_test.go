// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// Issue #639: borrowing a single cell span removes a staging allocation, not
// ownership at publication. Hidden capacity slots must still be snapshotted
// into indexed references, including cycles, before the source can change.
func TestTemplateBorrowedCellScratchDoesNotEscapePublication(t *testing.T) {
	source := templateOwnershipEnv()
	leaf := Int(17)
	backing := make([]*LVal, 2)
	subject := QExpr(backing[:0:2])
	backing[0], backing[1] = subject, leaf
	source.Runtime.Package.symbols["subject"] = subject
	inventory := newTemplateInventory(templateConfig{})
	if err := inventory.scan(source); err != nil {
		t.Fatal(err)
	}
	if len(inventory.cells) != 1 || inventory.cells[0].value != subject {
		t.Fatal("fixture must contain exactly one mutable cell span")
	}
	scratch := inventory.storage()
	if len(scratch.cells) != 1 || len(scratch.cells[0]) != 2 {
		t.Fatal("scratch storage lost the hidden capacity slots")
	}
	if &scratch.cells[0][0] != &backing[0] {
		t.Error("single-span scratch copied the backing instead of borrowing it")
	}
	if scratch.cells[0][0] != subject || scratch.cells[0][1] != leaf {
		t.Fatal("scratch storage changed the hidden cycle or leaf")
	}
	plan, err := compileTemplate(source, inventory)
	if err != nil {
		t.Fatal(err)
	}
	if len(plan.cells) != 1 || len(plan.cells[0]) != 2 {
		t.Fatal("compiled plan lost the capacity-tail references")
	}
	for slot, value := range []*LVal{subject, leaf} {
		index := inventory.values[value]
		ref := plan.cells[0][slot]
		if index <= 0 || ref.index != index || ref.shared != nil {
			t.Fatalf("slot %d did not become its admitted private index: got=%d want=%d shared=%t", slot, ref.index, index, ref.shared != nil)
		}
		if plan.values[index-1].header.Cells != nil {
			t.Fatalf("slot %d retained source cell backing in the plan header", slot)
		}
	}
	if len(subject.Cells) != 0 || cap(subject.Cells) != 2 || backing[0] != subject || backing[1] != leaf || leaf.Int != 17 {
		t.Fatal("publication changed source storage, bounds or contents")
	}
	tmpl := &Template{plan: plan}
	newSubject := func() *LVal {
		t.Helper()
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		value := vm.Get(Symbol("subject"))
		if value.Type != LSExpr || len(value.Cells) != 0 || cap(value.Cells) != 2 {
			t.Fatalf("VM changed the empty positive-capacity view: type=%v len=%d cap=%d", value.Type, len(value.Cells), cap(value.Cells))
		}
		slots := value.Cells[:cap(value.Cells)]
		if slots[0] != value || slots[1] == nil || slots[1].Type != LInt || slots[1].Int != 17 {
			t.Fatal("VM lost its private capacity-tail self-cycle or published leaf")
		}
		if value == subject || slots[1] == leaf || &slots[0] == &backing[0] {
			t.Fatal("VM retained mutable source identities")
		}
		return value
	}
	// Mutate both source objects and slots after publication, before any VM.
	leaf.Int = 99
	backing[0], backing[1] = Int(101), Int(102)
	first, second := newSubject(), newSubject()
	firstSlots, secondSlots := first.Cells[:2], second.Cells[:2]
	if first == second || firstSlots[1] == secondSlots[1] || &firstSlots[0] == &secondSlots[0] {
		t.Fatal("sibling VMs share mutable objects or cell storage")
	}
	firstSlots[1].Int = 41
	firstSlots[0], firstSlots[1] = Int(201), Int(202)
	if secondSlots[0] != second || secondSlots[1].Type != LInt || secondSlots[1].Int != 17 {
		t.Fatal("first VM mutation changed the existing sibling")
	}
	third := newSubject()
	thirdSlots := third.Cells[:2]
	if third == first || third == second || thirdSlots[1] == secondSlots[1] || &thirdSlots[0] == &firstSlots[0] || &thirdSlots[0] == &secondSlots[0] {
		t.Fatal("later VM reused an existing VM's mutable storage")
	}
	if leaf.Int != 99 || backing[0].Int != 101 || backing[1].Int != 102 || len(subject.Cells) != 0 || cap(subject.Cells) != 2 {
		t.Fatal("VM construction or mutation changed the already-modified source")
	}
}
