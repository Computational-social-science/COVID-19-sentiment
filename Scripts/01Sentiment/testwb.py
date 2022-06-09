from build_data import genDataLoader, convert_text_to_token
import torch
from transformers import BertTokenizer, BertModel
import torch.nn as nn
import json
import numpy as np

# Reuse model structure
class Model(nn.Module):
    def __init__(self, num_classes):
        super(Model, self).__init__()
        self.bert = BertModel.from_pretrained('chinese_wwm_ext_pytorch')  # /roberta-wwm-ext pretrain/
        for param in self.bert.parameters():
            param.requires_grad = True  
        self.fc = nn.Linear(768, num_classes)   # 768 -> 6
    def forward(self, x, token_type_ids, attention_mask):
        context = x  
        types = token_type_ids
        mask = attention_mask  
        _, pooled = self.bert(context, token_type_ids=types, attention_mask=mask)
        out = self.fc(pooled)   # get the probabilities of 2 classification 
        return out

PATH = '.\model\model.ckpt'
LABEL_DICT = {0:'negative', 1:'positive'} 
SEQ_LENGTH = 128
TOKENIZER = BertTokenizer.from_pretrained("./chinese_wwm_ext_pytorch") 
MODEL = Model(num_classes=2)
DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")
MODEL = MODEL.to(DEVICE)
MODEL.load_state_dict(torch.load(PATH))
print('The original model is loaded')

def test(model):
    with open('data/test.txt', encoding='utf8') as f:
        data = json.load(f)
    res = []
    correct = 0
    count = 0
    for each in data:
        cur_sentence = each['content']
        cur_label = each['label']
        ids = []
        types = []
        masks = []
        cur_ids, cur_type, cur_mask = convert_text_to_token(TOKENIZER, each['content'], seq_length=SEQ_LENGTH)
        ids.append(cur_ids)
        types.append(cur_type)
        masks.append(cur_mask)
        cur_ids, cur_type, cur_mask = torch.LongTensor(np.array(ids)).to(DEVICE), torch.LongTensor(np.array(types)).to(DEVICE), torch.LongTensor(np.array(masks)).to(DEVICE) # 数据构造成tensor形式
        with torch.no_grad():
            y_ = model(cur_ids, token_type_ids=cur_type, attention_mask=cur_mask)
            pred = y_.max(-1, keepdim=True)[1]  # 取最大值
            cur_pre = LABEL_DICT[int(pred[0][0].cuda().data.cpu().numpy())] # 预测的情绪
            if cur_label == cur_pre:
                correct += 1
        cur_res = cur_sentence + '\t' + cur_label + '\t' + cur_pre
        res.append(cur_res)
        count += 1
        if count % 1000 == 0:
            print('processed{}'.format(count))
    accu = correct / len(data)
    print('accu is {}'.format(accu))
    with open('testresult.txt', 'w', encoding='utf8') as f:
        for each in res:
            f.write(each+'\n')
if __name__ == '__main__':
    test(MODEL)