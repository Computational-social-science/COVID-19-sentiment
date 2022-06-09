import json
from transformers import BertTokenizer
from torch.utils.data import *

import torch
import numpy as np

SEQ_LENGTH = 128
BATCH_SIZE = 8
LABEL_DICT = {'negative':0, 'positive':1} 
TOKENIZER = BertTokenizer.from_pretrained("./chinese_wwm_ext_pytorch") 
TRAIN_DATA_PATH = './data/train.txt'
TEST_DATA_PATH = './data/eval.txt'

# Tokenization of data, seq_ Length indicates the maximum length of the accepted sentence
def convert_text_to_token(tokenizer, sentence, seq_length):
    tokens = tokenizer.tokenize(sentence) # Sentences are converted into tokens
    tokens = ["[CLS]"] + tokens + ["[SEP]"] # Add the [CLS]and[SEP] before and after token
    # Generate input_id, seg_id, att_mask
    ids1 = tokenizer.convert_tokens_to_ids(tokens)
    types = [0] * len(ids1)
    masks = [1] * len(ids1)
    if len(ids1) < seq_length:
        ids = ids1 + [0] * (seq_length - len(ids1))
        types = types + [1] * (seq_length - len(ids1))  
        masks = masks + [0] * (seq_length - len(ids1)) 
    else:
        ids = ids1[:seq_length]
        types = types[:seq_length]
        masks = masks[:seq_length]
    assert len(ids) == len(types) == len(masks)
    return ids, types, masks

# Construct the DataLoader for the training and test sets
def genDataLoader(is_train):
    if is_train: 
        path = TRAIN_DATA_PATH
    else: 
        path = TEST_DATA_PATH
    with open(path, encoding='utf8') as f:
        data = json.load(f)
    ids_pool = []
    types_pool = []
    masks_pool = []
    target_pool = []
    count = 0
    
    for each in data:
        cur_ids, cur_type, cur_mask = convert_text_to_token(TOKENIZER, each['content'], seq_length = SEQ_LENGTH)
        ids_pool.append(cur_ids)
        types_pool.append(cur_type)
        masks_pool.append(cur_mask)
        cur_target = LABEL_DICT[each['label']]
        target_pool.append([cur_target])
        count += 1
        if count % 1000 == 0:
            print('processed{}'.format(count))
            
    
    data_gen = TensorDataset(torch.LongTensor(np.array(ids_pool)),
                               torch.LongTensor(np.array(types_pool)),
                               torch.LongTensor(np.array(masks_pool)),
                               torch.LongTensor(np.array(target_pool)))
   
    sampler = RandomSampler(data_gen)
    loader = DataLoader(data_gen, sampler=sampler, batch_size=BATCH_SIZE)
    return loader
