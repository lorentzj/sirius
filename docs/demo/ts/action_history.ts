export interface Action<S, P> {
    inverse: () => Action<S, P>;
    execute: () => boolean;
}

export class CompositeAction<S, P> implements Action<S, P> {
    private actionList: Action<S, P>[];
    constructor(actionList: Action<S, P>[]) {
        this.actionList = actionList;
    }

    inverse(): CompositeAction<S, P> {
        return new CompositeAction<S, P>(this.actionList.reverse().map(a => a.inverse()));
    }

    execute(): boolean {
        return this.actionList.every(a => a.execute());
    }
}

export class ActionHistory<T> {
    private history: Action<unknown, T>[];
    private currentAction: number;
    private maxHistoryLength: number;

    constructor(maxHistoryLength: number) {
        this.history = [];
        this.currentAction = -1;
        this.maxHistoryLength = maxHistoryLength;
    }

    executeAction(action: Action<unknown, T>): boolean {
        const success = action.execute();

        if(success) {
            if(this.currentAction + 1 < this.history.length) {
                this.history = this.history.slice(undefined, this.currentAction + 1);
            }

            this.history.push(action);

            if(this.history.length > this.maxHistoryLength) {
                this.history.shift();
            } else {
                this.currentAction += 1;
            }
        }

        return success;
    }

    undoAction(): boolean {
        if(this.currentAction >= 0) {
            const r = this.history[this.currentAction].inverse().execute();
            this.currentAction -= 1;
            return r;
        } else {
            return false;
        }
    }

    redoAction(): boolean {
        if(this.currentAction + 1 < this.history.length) {
            this.currentAction += 1;
            return this.history[this.currentAction].execute();
        } else {
            return true;
        }
    }
}