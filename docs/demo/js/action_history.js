export class CompositeAction {
    constructor(actionList) {
        this.actionList = actionList;
    }
    inverse() {
        return new CompositeAction(this.actionList.reverse().map(a => a.inverse()));
    }
    execute() {
        return this.actionList.every(a => a.execute());
    }
}
export class ActionHistory {
    constructor(maxHistoryLength) {
        this.history = [];
        this.currentAction = -1;
        this.maxHistoryLength = maxHistoryLength;
    }
    executeAction(action) {
        const success = action.execute();
        if (success) {
            if (this.currentAction + 1 < this.history.length) {
                this.history = this.history.slice(undefined, this.currentAction + 1);
            }
            this.history.push(action);
            if (this.history.length > this.maxHistoryLength) {
                this.history.shift();
            }
            else {
                this.currentAction += 1;
            }
        }
        return success;
    }
    undoAction() {
        if (this.currentAction >= 0) {
            const r = this.history[this.currentAction].inverse().execute();
            this.currentAction -= 1;
            return r;
        }
        else {
            return false;
        }
    }
    redoAction() {
        if (this.currentAction + 1 < this.history.length) {
            this.currentAction += 1;
            return this.history[this.currentAction].execute();
        }
        else {
            return true;
        }
    }
}
