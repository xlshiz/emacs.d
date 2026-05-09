#!/bin/bash

# --- 配置 ---
# 要尝试切换的分支列表（按优先级从高到低），脚本会尝试找到第一个存在的分支
BRANCH_PRIORITIES=("main" "master")
# -----------------

echo "Starting submodule update script (without auto-adding to main repo)..."

# 确保当前在 Git 仓库根目录
if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    echo "Error: Not inside a Git work tree. Please run this script from the root of your main repository."
    exit 1
fi

MAIN_REPO_ROOT=$(git rev-parse --show-toplevel)
echo "Main repository root: $MAIN_REPO_ROOT"
cd "$MAIN_REPO_ROOT" || { echo "Failed to change to main repo root."; exit 1; }

# 获取所有子模块的路径
SUBMODULE_PATHS=$(git config --file .gitmodules --get-regexp path | awk '{ print $2 }')

if [ -z "$SUBMODULE_PATHS" ]; then
    echo "No submodules found in .gitmodules."
    exit 0
fi

UPDATED_SUBMODULES=()

for SUBMODULE_PATH in $SUBMODULE_PATHS; do
    echo -e "\n--- Processing submodule: $SUBMODULE_PATH ---"

    # 检查子模块目录是否存在
    if [ ! -d "$SUBMODULE_PATH" ]; then
        echo "Warning: Submodule directory '$SUBMODULE_PATH' not found or not initialized. Skipping."
        echo "You might need to run 'git submodule update --init --recursive' first."
        continue
    fi

    pushd "$SUBMODULE_PATH" > /dev/null || { echo "Failed to enter submodule directory '$SUBMODULE_PATH'. Skipping."; continue; }

    if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        echo "Error: '$SUBMODULE_PATH' is not a Git repository. Please initialize/update it first. Skipping."
        popd > /dev/null
        continue
    fi

    # 清理潜在的本地更改，防止 pull 失败。
    # 警告：这会丢弃所有子模块内的未提交更改！请谨慎使用或注释掉。
    # git reset --hard > /dev/null 2>&1
    # git clean -fd > /dev/null 2>&1
    # echo "Cleared any uncommitted changes in $SUBMODULE_PATH."

    echo "Fetching all remotes in $SUBMODULE_PATH..."
    git fetch --all --tags --prune

    TARGET_BRANCH=""
    for branch_name in "${BRANCH_PRIORITIES[@]}"; do
        if git rev-parse --verify "remotes/origin/$branch_name" >/dev/null 2>&1; then
            TARGET_BRANCH="$branch_name"
            break
        fi
    done

    if [ -z "$TARGET_BRANCH" ]; then
        echo "Could not find any of the preferred branches (${BRANCH_PRIORITIES[*]}) in remote 'origin' for $SUBMODULE_PATH. Skipping update."
        popd > /dev/null
        continue
    fi

    echo "Attempting to switch to branch '$TARGET_BRANCH' and pull latest."
    if git checkout "$TARGET_BRANCH" > /dev/null 2>&1; then
        echo "Successfully switched to branch '$TARGET_BRANCH'."
        # 确保追踪远程分支
        if [ "$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)" != "origin/$TARGET_BRANCH" ]; then
            git branch --set-upstream-to=origin/"$TARGET_BRANCH" "$TARGET_BRANCH" > /dev/null 2>&1
            echo "Set upstream for '$TARGET_BRANCH' to 'origin/$TARGET_BRANCH'."
        fi

        CURRENT_COMMIT_BEFORE_PULL=$(git rev-parse HEAD)
        git pull origin "$TARGET_BRANCH"

        if [ $? -eq 0 ]; then
            CURRENT_COMMIT_AFTER_PULL=$(git rev-parse HEAD)
            if [ "$CURRENT_COMMIT_BEFORE_PULL" != "$CURRENT_COMMIT_AFTER_PULL" ]; then
                echo "Submodule '$SUBMODULE_PATH' updated from $CURRENT_COMMIT_BEFORE_PULL to $CURRENT_COMMIT_AFTER_PULL."
                UPDATED_SUBMODULES+=("$SUBMODULE_PATH") # 记录更新的子模块
            else
                echo "Submodule '$SUBMODULE_PATH' was already up to date on '$TARGET_BRANCH'."
            fi
        else
            echo "Error: Failed to pull latest changes for '$SUBMODULE_PATH' on '$TARGET_BRANCH'. You may need to resolve conflicts manually."
        fi
    else
        echo "Error: Failed to checkout branch '$TARGET_BRANCH' in $SUBMODULE_PATH. Skipping update."
    fi

    popd > /dev/null # 返回主仓库目录

done

echo -e "\n--- All submodules processed ---"

if [ ${#UPDATED_SUBMODULES[@]} -gt 0 ]; then
    echo "The following submodules were updated and their main repository references might need to be adjusted manually:"
    for sm in "${UPDATED_SUBMODULES[@]}"; do
        echo "  - $sm"
    done
    echo ""
    echo "To update the main repository's reference for these submodules, run:"
    echo "  git add ${UPDATED_SUBMODULES[*]}"
    echo "Then commit: git commit -m \"Update specific submodules to latest $BRANCH_PRIORITIES[0]/${BRANCH_PRIORITIES[1]} references\""
else
    echo "No submodules were updated."
fi
