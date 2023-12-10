module PlutusTx.MerkleBST where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.ByteString -- Assuming appropriate import for ByteString

-- Your provided Hash-related definitions and functions
newtype Hash = Hash BuiltinByteString deriving (Eq)

hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256
{-# INLINEABLE hash #-}

combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

combineFourHashes :: Hash -> Hash -> Hash -> Hash -> Hash
combineFourHashes h1 h2 h3 h4 = hash (appendByteString (appendByteString h1 h2) (appendByteString h3 h4))
{-# INLINEABLE combineFourHashes #-}

newtype NodeHash = NodeHash Hash
  deriving (Eq)

newtype KeyHash = KeyHash Hash
  deriving (Eq, Ord)

newtype ValueHash = ValueHash Hash
  deriving (Eq, Ord)

newtype KeyValueHash = KeyValueHash Hash
  deriving (Eq, Ord)

data Tree k v = Empty | Node k (Value v) NodeHash (Tree k v) (Tree k v)
    deriving (Show, Eq)

emptyNodeHash :: NodeHash
emptyNodeHash = NodeHash (hash "")

-- Compute the root hash
rootHash :: Tree k v -> NondeHash
rootHash Empty = emptyNodeHash
rootHash (Node _ _ h _ _) = h

mkKeyValueHash :: KeyHash -> ValueHash -> KeyValueHash
mkKeyValueHash (KeyHash keyHash) (ValueHash valueHash) = KeyValueHash (hash (keyHash `appendByteString` valueHash))

mkNodeHash :: KeyValueHash -> NodeHash -> NodeHash -> NodeHash
mkNodeHash (KeyValueHash keyValueHash) (NodeHash leftHash) (NodeHash rightHash)
  = Nodehash $ hash (leftHash `appendByteString` keyValueHash `appendByteString` rightHash)

-- Let's turn the above into mkNode
mkNode :: (v -> ValueHash) -> KeyHash -> k -> v -> Tree k v -> Tree k v -> Tree k v
mkNode hashV (KeyHash keyHash) k v left right = do
  let
    leftHash = rootHash left
    rightHash = rootHash right
    keyValueHash = mkKeyValueHash keyHash (hashV v)
    nodeHash = mkNodeHash keyValueHash leftHash rightHash
  Node k v nodeHash left right

-- Construct bianary tree sorted against hash of the keys
mkTree :: Ord k => (k -> Hash) -> (v -> Hash) -> [(k, v)] -> Maybe (Tree k v)
mkTree hashK hashV kvs = Just $ go sorted
  where
    -- add hash to the key-value pairs
    sorted = do
      let
        kvs' =  kvs <&> \(k, v) -> (hashk k, k, v)
      sortBy (comparing fst) kvs'

    mkNode' = mkNode hashV

    go [] = Empty
    go [(kh, k, v)] = mkNode kh k v Empty Empty
    go [(kh1, k1, v1), (kh2 k2, v2)] = do
      let
        right = mkNode kh1 k2 v2 Empty Empty
      mkNode kh2 k1 v1 Empty right
    go [(kh1, k1, v1), (kh2, k2, v2), (kh3, k3, v3)] = do
      let
        left = mkNode kh1 k1 v1 Empty Empty
        right = mkNode kh3 k3 v3 Empty Empty
      mkNode kh2 k2 v2 left right
    go kvs = do
      let
        (preceding, (kh, k, v):succeeding) = splitAt (length kvs `div` 2) kvs
        left = go preceding
        right = go succeeding
      mkNode kh k v left right

-- * Assumption: the tree is sorted by the hash of the keys. We don't need to check this.
-- * We provide optional hashes of the children of the inserted node.
-- * The list should be provided in the leaf to root order.
-- * `Left` indicates that we are going right and left subtree hash is provided.
-- * `Right` indicates that we are going left and right subtree hash is provided.
-- * For every node we provide the hash of the key-value pair as well as the hash of the sibling node.
type MembershipProof = ((Maybe NodeHash, Maybe NodeHash), [Either (NodeHash, KeyValueHash)])

newtype RootHash = RootHash NodeHash deriving (Eq)

member :: (k -> KeyHash) -> (v -> ValueHash) -> k -> v -> RootHash -> MembershipProof -> Boolean
member hashK hashV k v (RootHash rootHash) ((l, r), proof) = do
  let
    leftHash = fromMaybe emptyNodeHash l
    rightHash = fromMaybe emptyNodeHash r
    kh = hashK k
    nodeHash = mkNodeHash (mkKeyValueHash kh (hashV v)) leftHash rightHash

    go childHash [] = childHash == rootHash
    go childHash (Left (siblingHash, keyValueHash):xs) = do
      let
        nodeHash' = mkNodeHash keyValueHash siblingHash childHash
      go nodeHash' xs
    go childHash (Right (siblingHash, keyValueHash):xs) = do
      let
        nodeHash' = mkNodeHash keyValueHash childHash siblingHash
      go nodeHash' xs
  go nodeHash proof

newtype PrevRootHash = PrevRootHash NodeHash deriving (Eq)
newtype NextRootHash = NextRootHash NodeHash deriving (Eq)

update :: (k -> KeyHash) -> (v -> ValueHash) -> k -> ValueHash -> v -> PrevRootHash -> NextRootHash -> MembershipProof -> Boolean
update hashK hashV k prevValueHash v (PrevRootHash prevRootHash) (NextRootHash nextRootHash) ((l, r), proof) = do
  let
    leftHash = fromMaybe emptyNodeHash l
    rightHash = fromMaybe emptyNodeHash r
    kh = hashK k

    prevKeyValueHash = mkKeyValueHash kh prevValueHash
    nextKeyValueHash = mkKeyValueHash kh (hashV v)

    prevNodeHash = mkNodeHash prevKeyValueHash leftHash rightHash
    nextNodeHash = mkNodeHash nextKeyValueHash leftHash rightHash

    go prevChildHash nextChildHash [] = prevChildHash == prevRootHash && nextChildHash == nextRootHash
    go prevChildHash nextChildHash (Left (siblingHash, keyValueHash):xs) = do
      let
        prevNodeHash' = mkNodeHash keyValueHash siblingHash prevChildHash
        nextNodeHash' = mkNodeHash keyValueHash siblingHash nextChildHash
      go prevNodeHash' nextNodeHash' xs
    go prevChildHash nextChildHash (Right (siblingHash, keyValueHash):xs) = do
      let
        prevNodeHash' = mkNodeHash keyValueHash prevChildHash siblingHash
        nextNodeHash' = mkNodeHash keyValueHash nextChildHash siblingHash
      go prevNodeHash' nextNodeHash' xs
  go prevNodeHash nextNodeHash proof

-- * In this case we have to prove that the key is not in the tree.
-- * We prove this by checking the invariant across the path from the leaf to the root.
-- * We provide optional hash of the existing sibling.
-- * In the case of the last node which has no children the node hash should be `emptyNodeHash`.
type InsertionProof = ([Either (NodeHash, KeyHash, ValueHash)])

insert :: (k -> KeyHash) -> (v -> ValueHash) -> k -> v -> PrevRootHash -> NextRootHash -> InsertionProof -> Boolean
insert hashK hashV k v (PrevRootHash prevRootHash) (NextRootHash nextRootHash) (s, proof) = do
  let
    kh = hashK k
    kvh = mkKeyValueHash kh (hashV v)
    -- New node
    prevNodeHash = emptyNodeHash
    newNodeHash = mkNodeHash kvh emptyNodeHash emptyNodeHash

    go prevChildHash nextChildHash [] = prevChildHash == prevRootHash && nextChildHash == nextRootHash
    go prevChildHash nextChildHash (Left (siblingHash, keyHash, valueHash):xs) = do
      if keyHash >= kh
        then error "BST invariant violated"
        else do
          let
            kvh = mkKeyValueHash keyHash valueHash
            prevNodeHash' = mkNodeHash kvh siblingHash prevChildHash
            nextNodeHash' = mkNodeHash kvh siblingHash nextChildHash
          go prevNodeHash' nextNodeHash' xs
    go prevChildHash nextChildHash (Right (siblingHash, keyHash, valueHash):xs) = do
      if keyHash <= kh
        then error "BST invariant violated"
        else do
          let
            kvh = mkKeyValueHash keyHash valueHash
            prevNodeHash' = mkNodeHash kvh prevChildHash siblingHash
            nextNodeHash' = mkNodeHash kvh nextChildHash siblingHash
          go prevNodeHash' nextNodeHash' xs
  go prevNodeHash newNodeHash proof
